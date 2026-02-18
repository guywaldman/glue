import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, TransportKind, Executable, State } from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let extensionDiagnostics: vscode.DiagnosticCollection;
let extensionErrorMessage: string | undefined;

function formatError(err: unknown): string {
  if (err instanceof Error) {
    return err.message;
  }
  return String(err);
}

function makeExtensionDiagnostic(message: string): vscode.Diagnostic {
  const range = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 1));
  const diagnostic = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error);
  diagnostic.source = "glue-extension";
  return diagnostic;
}

function applyExtensionDiagnosticsForOpenGlueFiles() {
  if (!extensionErrorMessage) {
    return;
  }

  const diagnostic = makeExtensionDiagnostic(extensionErrorMessage);
  for (const document of vscode.workspace.textDocuments) {
    if (document.languageId === "glue" && document.uri.scheme === "file") {
      extensionDiagnostics.set(document.uri, [diagnostic]);
    }
  }
}

function setExtensionErrorDiagnostics(message: string) {
  extensionErrorMessage = message;
  applyExtensionDiagnosticsForOpenGlueFiles();
}

function clearExtensionErrorDiagnostics() {
  extensionErrorMessage = undefined;
  extensionDiagnostics.clear();
}

export function activate(context: vscode.ExtensionContext) {
  extensionDiagnostics = vscode.languages.createDiagnosticCollection("glue-extension");
  context.subscriptions.push(extensionDiagnostics);

  const diagnosticsOnOpen = vscode.workspace.onDidOpenTextDocument((document) => {
    if (!extensionErrorMessage) {
      return;
    }
    if (document.languageId !== "glue" || document.uri.scheme !== "file") {
      return;
    }
    extensionDiagnostics.set(document.uri, [makeExtensionDiagnostic(extensionErrorMessage)]);
  });
  context.subscriptions.push(diagnosticsOnOpen);

  const serverExecutable = process.platform === "win32" ? "lsp.exe" : "lsp";
  const serverPath = path.join(context.extensionPath, "out", serverExecutable);
  console.log(`Glue LSP resolved path: ${serverPath}`);
  if (!fs.existsSync(serverPath)) {
    const message = "Glue LSP binary not found. Build with 'cargo build -p lsp --release'.";
    setExtensionErrorDiagnostics(message);
    vscode.window.showErrorMessage(message);
    return;
  }

  const run: Executable = {
    command: serverPath,
    transport: TransportKind.stdio,
    options: { env: { ...process.env, RUST_LOG: "debug" } },
  };
  const serverOptions = { run, debug: run };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "glue" }],
    synchronize: { fileEvents: vscode.workspace.createFileSystemWatcher("**/*.glue") },
  };

  client = new LanguageClient("glueLsp", "Glue Language Server", serverOptions, clientOptions);
  context.subscriptions.push(
    client.onDidChangeState((event) => {
      if (event.newState === State.Running) {
        clearExtensionErrorDiagnostics();
        return;
      }

      if (event.oldState === State.Running && event.newState === State.Stopped) {
        const message = "Glue language server stopped unexpectedly. Run 'Glue: Restart Language Server'.";
        setExtensionErrorDiagnostics(message);
        void vscode.window.showErrorMessage(message, "Restart", "Show Logs").then(async (selection) => {
          if (selection === "Restart") {
            await vscode.commands.executeCommand("glue.restart");
          }
          if (selection === "Show Logs") {
            await vscode.commands.executeCommand("glue.showLogs");
          }
        });
      }
    })
  );

  const disposable = {
    dispose: () => {
      client?.stop();
    },
  };
  client
    .start()
    .then(() => {
      clearExtensionErrorDiagnostics();
      console.log("Glue LSP started");
      context.subscriptions.push(disposable);
    })
    .catch((err) => {
      const message = `Failed to start Glue LSP: ${formatError(err)}`;
      setExtensionErrorDiagnostics(message);
      vscode.window.showErrorMessage(message);
    });

  const restartCommand = vscode.commands.registerCommand("glue.restart", async () => {
    if (client) {
      try {
        await client.stop();
        await client.start();
        clearExtensionErrorDiagnostics();
        vscode.window.showInformationMessage("Glue LSP restarted");
      } catch (err) {
        const message = `Failed to restart Glue LSP: ${formatError(err)}`;
        setExtensionErrorDiagnostics(message);
        vscode.window.showErrorMessage(message);
      }
    }
  });

  const showLogsCommand = vscode.commands.registerCommand("glue.showLogs", async () => {
    if (client) {
      client.outputChannel.show();
    }
  });

  context.subscriptions.push(restartCommand, showLogsCommand);
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop();
  }
  return undefined;
}
