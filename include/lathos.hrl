-record(node, { id, term }).

% returned by the lathos_db:subtree/1
-record (tree, {node, children}).