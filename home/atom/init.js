atom.commands.add('atom-workspace', 'init:merge-panes', () => {
  const aPanes = atom.workspace.getCenter().getPanes();
  const oFirstPane = aPanes.shift();
  return Array.from(aPanes).map((oPane) =>
  Array.from(oPane.getItems()).map((oItem) =>
      oPane.moveItemToPane(oItem, oFirstPane)));
});
