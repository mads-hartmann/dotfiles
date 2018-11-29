atom.commands.add('atom-workspace', 'init:merge-panes', () => {
  const aPanes = atom.workspace.getCenter().getPanes();
  const oFirstPane = aPanes.shift();
  return Array.from(aPanes).map(oPane =>
    Array.from(oPane.getItems()).map(oItem =>
      oPane.moveItemToPane(oItem, oFirstPane)
    )
  );
});

atom.commands.add('atom-text-editor', 'init:capitalize-word', () => {
  const editor = atom.workspace.getActiveTextEditor();
  if (!editor) {
    return;
  }
  editor.getCursors().forEach(cursor => {
    const range = cursor.getCurrentWordBufferRange();
    const text = editor.getTextInBufferRange(range);
    const capitalized = text.charAt(0).toUpperCase() + text.slice(1);
    editor.setTextInBufferRange(range, capitalized);
  });
});
