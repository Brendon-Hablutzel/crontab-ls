import cron.*
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.*

import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

def diagnoseCronError(cronError: CronError): Diagnostic = {
  val range =
    new Range(
      new Position(cronError.line, cronError.startChar),
      new Position(cronError.line, cronError.endChar)
    )

  new Diagnostic(
    range,
    cronError.message,
    DiagnosticSeverity.Error,
    "Crontab Language Server"
  )
}

//  deltaLine: the line difference between this token and the previous
//  deltaStartChar: either the start character difference between this token and the previous
//    (if the previous is on the same line), or just the start character of this token
//    (if the previous is on a different line)
//  length: the length of this token
//  tokenType: index into the token type legend
//  tokenModifiers: bit flags for token modifiers
case class SemanticToken(
    deltaLine: Int,
    deltaStartChar: Int,
    length: Int,
    tokenType: String
):
  def toIntegerArray: Array[Int] = {
    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
    // https://stackoverflow.com/questions/70490767/language-server-semantic-tokens
    Array(
      deltaLine,
      deltaStartChar,
      length,
      SemanticToken.tokenToIndex(tokenType),
      0
    )
  }

object SemanticToken:
  val tokenLegend: List[String] = List(
    SemanticTokenTypes.Variable,
    SemanticTokenTypes.Function,
    SemanticTokenTypes.Comment
  )

  private def tokenToIndex(token: String): Int = {
    val index = tokenLegend.indexOf(token, 0)

    if index < 0 then
      throw new UnsupportedOperationException(
        s"semantic token $token is not supported"
      )

    index
  }

def parseCrontabFile(content: String): Vector[CrontabToken] = {
  val parser = new CrontabParser
  parser.parseFile(content)
}

def tokensToSemanticTokens(
    tokens: Vector[CrontabToken]
): Vector[SemanticToken] = {
  var lastLine = 0
  var lastChar = 0

  tokens.map { token =>
    val (lineIdx, charIdx, length, tokenTypeStr) = token match
      case CrontabToken.Comment(lineIdx, content) =>
        (lineIdx, 0, content.length, SemanticTokenTypes.Comment)
      case CrontabToken.Term(lineIdx, charIdx, _, _, content) =>
        (lineIdx, charIdx, content.length, SemanticTokenTypes.Variable)
      case CrontabToken.Command(lineIdx, charIdx, content) =>
        (lineIdx, charIdx, content.length, SemanticTokenTypes.Function)

    val deltaLine = lineIdx - lastLine
    val deltaChar =
      if (deltaLine == 0) charIdx - lastChar
      else charIdx

    lastLine = lineIdx
    lastChar = charIdx

    SemanticToken(deltaLine, deltaChar, length, tokenTypeStr)
  }
}

def getErrorsFromTokens(tokens: Vector[CrontabToken]): Vector[CronError] = {
  tokens.collect {
    case CrontabToken.Term(lineIndex, charIndex, _, Left(cronTermErrors), _) =>
      cronTermErrors.map(termError =>
        CronError(
          lineIndex,
          termError.startChar + charIndex,
          termError.endChar + charIndex,
          termError.message
        )
      )
  }.flatten
}

class CrontabLS extends LanguageServer with LanguageClientAware {
  // key is uri
  private val openFiles = mutable.Map.empty[String, Vector[CrontabToken]]

  private val textDocumentService = new TextDocumentService {
    private def processFile(uri: String, content: String): Unit = {
      val tokens = parseCrontabFile(content)

      Console.err.println(tokens)

      // track open files
      openFiles.update(uri, tokens)

      // get errors
      val errors = getErrorsFromTokens(tokens)
      sendSyntaxErrors(uri, errors)
    }

    override def didOpen(params: DidOpenTextDocumentParams): Unit = {
      Console.err.println(s"Document opened: ${params.getTextDocument.getUri}")

      val content = params.getTextDocument.getText
      val uri = params.getTextDocument.getUri

      processFile(uri, content)
    }

    override def didChange(params: DidChangeTextDocumentParams): Unit = {
      Console.err.println(s"Document changed: ${params.getTextDocument.getUri}")
      Console.err.println(s"Get content changes: ${params.getContentChanges}")

      val uri = params.getTextDocument.getUri
      val newContent = params.getContentChanges.asScala.map(_.getText).mkString

      processFile(uri, newContent)
    }

    override def didClose(params: DidCloseTextDocumentParams): Unit = {
      Console.err.println(s"Document closed: ${params.getTextDocument.getUri}")
      val uri = params.getTextDocument.getUri
      openFiles -= uri
    }

    override def didSave(params: DidSaveTextDocumentParams): Unit = {
      Console.err.println(s"Document saved: ${params.getTextDocument.getUri}")
    }

    private def getHoverText(
        line: Vector[CrontabToken],
        hovered: CrontabToken
    ): Option[String] = {
      hovered match {
        case CrontabToken.Comment(_, _) =>
          None
        case CrontabToken.Term(_, _, termType, Right(termValue), _) =>
          Some(s"${termType.toString} term: ${termValue.toString}")
        case CrontabToken.Term(_, _, _, Left(_), _) =>
          // TODO: any text?
          None
        case CrontabToken.Command(_, _, content) =>
          // TODO: get human-readable schedule
          Some(s"`$content` will be executed ${line.take(5)}")
      }
    }

    override def hover(params: HoverParams): CompletableFuture[Hover] = {
      val uri = params.getTextDocument.getUri
      val pos = params.getPosition
      val hoveredLineIndex = pos.getLine
      val hoveredCharIndex = pos.getCharacter

      val tokens = openFiles(uri)

      val lineTokens = tokens.collect {
        case token @ CrontabToken.Comment(lineIndex, _)
            if lineIndex == hoveredLineIndex =>
          token
        case token @ CrontabToken.Term(lineIndex, _, _, _, _)
            if lineIndex == hoveredLineIndex =>
          token
        case token @ CrontabToken.Command(lineIndex, _, _)
            if lineIndex == hoveredLineIndex =>
          token
      }

      val highlightedToken = lineTokens.find { token =>
        val (startIndex, endIndex) = token.getCharRange
        hoveredCharIndex >= startIndex && hoveredCharIndex < endIndex
      }

      val hoverContent =
        highlightedToken
          .flatMap(highlightedToken =>
            getHoverText(lineTokens, highlightedToken).map { text =>
              val markup = new MarkupContent(MarkupKind.PLAINTEXT, text)

              val (startCharIndex, endCharIndex) = highlightedToken.getCharRange

              val range = new Range(
                new Position(hoveredLineIndex, startCharIndex),
                new Position(hoveredLineIndex, endCharIndex)
              )

              new Hover(
                markup,
                range
              )
            }
          )
          .orNull

      CompletableFuture.completedFuture(hoverContent)
    }

    override def semanticTokensFull(
        params: SemanticTokensParams
    ): CompletableFuture[SemanticTokens] = {
      val tokens = openFiles(params.getTextDocument.getUri)
      val semanticTokens = tokensToSemanticTokens(tokens)
        .flatMap(_.toIntegerArray)
        .map(java.lang.Integer.valueOf)
        .asJava

      CompletableFuture.completedFuture(
        new SemanticTokens(semanticTokens)
      )
    }
  }

  private val workspaceService = new WorkspaceService {
    override def didChangeConfiguration(
        params: DidChangeConfigurationParams
    ): Unit = {}

    override def didChangeWatchedFiles(
        params: DidChangeWatchedFilesParams
    ): Unit = {}
  }

  private def sendSyntaxErrors(
      uri: String,
      cronErrors: Vector[CronError]
  ): Unit = {
    val diagnostics = cronErrors.map(diagnoseCronError).asJava

    val publishParams = new PublishDiagnosticsParams(uri, diagnostics)
    client.publishDiagnostics(publishParams)
  }

  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    Console.err.println("Server initialized")

    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setHoverProvider(true)

    val semanticTokensLegend = new SemanticTokensLegend(
      SemanticToken.tokenLegend.asJava,
      List().asJava
    )
    val semanticTokensProvider = new SemanticTokensWithRegistrationOptions(
      semanticTokensLegend,
      true, // full support
      false // no range support
    )
    capabilities.setSemanticTokensProvider(semanticTokensProvider)

    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def shutdown(): CompletableFuture[Object] = {
    Console.err.println("Server shutting down")

    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {
    Console.err.println("Server exited")
  }

  override def getTextDocumentService: TextDocumentService =
    textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService

  private var client: LanguageClient = scala.compiletime.uninitialized
  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }
}

@main def main(): Unit =
  val server = new CrontabLS()

  val launcher = LSPLauncher.createServerLauncher(
    server,
    System.in,
    System.out
  )

  server.connect(launcher.getRemoteProxy)
  Console.err.println("Language server listening")
  launcher.startListening()
