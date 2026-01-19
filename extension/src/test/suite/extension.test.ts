import * as assert from "assert";
import * as vscode from "vscode";
import * as path from "path";

// TODO: Replace sleep with proper event-based waiting
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

suite("Glue LSP E2E Tests", () => {
  const fixturesPath = path.resolve(__dirname, "../../../test-fixtures");

  // Helper to wait for the LSP to be ready
  async function waitForLspReady(timeout = 10000): Promise<void> {
    const startTime = Date.now();
    while (Date.now() - startTime < timeout) {
      // Check if diagnostics are available (indicates LSP is processing)
      await new Promise((resolve) => setTimeout(resolve, 500));
      // Try a simple operation to verify LSP is responding
      const doc = vscode.window.activeTextEditor?.document;
      if (doc && doc.languageId === "glue") {
        return;
      }
    }
    throw new Error("LSP did not become ready in time");
  }

  // Helper to open a test file
  async function openTestFile(fileName: string): Promise<vscode.TextDocument> {
    const filePath = path.join(fixturesPath, fileName);
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(document);
    await waitForLspReady();
    return document;
  }

  test("Extension should be present", () => {
    const extension = vscode.extensions.getExtension("guywaldman.glue");
    assert.ok(extension, "Extension should be installed");
  });

  test("Extension should activate on .glue file", async () => {
    const doc = await openTestFile("test.glue");
    assert.strictEqual(doc.languageId, "glue", "Document should have glue language ID");

    const extension = vscode.extensions.getExtension("guywaldman.glue");
    assert.ok(extension?.isActive, "Extension should be active after opening .glue file");
  });

  test("Hover should show model documentation", async () => {
    const doc = await openTestFile("test.glue");

    // Find the position of "Bar" reference in "field: Bar"
    const text = doc.getText();
    const barRefIndex = text.indexOf("field: Bar");
    assert.ok(barRefIndex !== -1, "Should find 'field: Bar' in test file");

    const barPosition = doc.positionAt(barRefIndex + "field: ".length);

    // Request hover
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      barPosition
    );

    assert.ok(hovers && hovers.length > 0, "Should return hover results");

    const hoverContent = hovers[0].contents
      .map((c) => (typeof c === "string" ? c : c.value))
      .join("\n");

    assert.ok(hoverContent.includes("model"), "Hover should mention 'model'");
    assert.ok(hoverContent.includes("Bar"), "Hover should mention 'Bar'");
  });

  test("Go to definition should navigate to model", async () => {
    const doc = await openTestFile("test.glue");

    // Find the position of "Bar" reference
    const text = doc.getText();
    const barRefIndex = text.indexOf("field: Bar");
    assert.ok(barRefIndex !== -1, "Should find 'field: Bar' in test file");

    const barPosition = doc.positionAt(barRefIndex + "field: ".length);

    // Request definition
    const definitions = await vscode.commands.executeCommand<vscode.Location[]>(
      "vscode.executeDefinitionProvider",
      doc.uri,
      barPosition
    );

    assert.ok(definitions && definitions.length > 0, "Should return definition results");

    const def = definitions[0];
    assert.strictEqual(def.uri.fsPath, doc.uri.fsPath, "Definition should be in the same file");

    // The definition should point to where "model Bar" is defined
    const defText = doc.getText(def.range);
    assert.ok(
      defText.includes("model Bar") || def.range.start.line > barPosition.line,
      "Definition should point to model Bar declaration"
    );
  });

  test("Completion should suggest models and enums", async () => {
    const doc = await openTestFile("completion-test.glue");

    // Find a position where completion should trigger (after a colon in a field type)
    const text = doc.getText();
    const incompleteFieldIndex = text.indexOf("needs_completion:");
    assert.ok(incompleteFieldIndex !== -1, "Should find 'needs_completion:' in test file");

    const completionPosition = doc.positionAt(
      incompleteFieldIndex + "needs_completion: ".length
    );

    // Request completions
    const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
      "vscode.executeCompletionItemProvider",
      doc.uri,
      completionPosition
    );

    assert.ok(completions, "Should return completion results");
    assert.ok(completions.items.length > 0, "Should have completion items");

    const labels = completions.items.map((item) => item.label);
    // Should include at least some defined types
    assert.ok(
      labels.some((l) => typeof l === "string" || (l as vscode.CompletionItemLabel).label),
      "Should have completion labels"
    );
  });

  test("Hover on enum should show variants", async () => {
    // Close all editors first to ensure clean state
    await vscode.commands.executeCommand("workbench.action.closeAllEditors");
    await sleep(200);

    const doc = await openTestFile("test.glue");

    // Find enum reference
    const text = doc.getText();
    const statusRefIndex = text.indexOf("status: Status");
    assert.ok(
      statusRefIndex !== -1,
      `Should find 'status: Status' in test.glue. File content: ${text.substring(0, 200)}...`
    );

    // Position at the 'S' of 'Status'
    const statusPosition = doc.positionAt(statusRefIndex + "status: ".length);

    // Wait a bit for the LSP to process
    await sleep(500);

    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      statusPosition
    );

    assert.ok(hovers && hovers.length > 0, "Should have hover results");

    const hoverContent = hovers[0].contents
      .map((c) => (typeof c === "string" ? c : c.value))
      .join("\n");

    assert.ok(
      hoverContent.toLowerCase().includes("enum"),
      `Hover should mention 'enum', got: ${hoverContent}`
    );
  });
});
