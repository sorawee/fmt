type tree =
  | Leaf
  | Node(int, tree, tree);

let f = (x, y) => x + y;

let rec sum = item =>
  switch (item) {
  | Leaf => 0
  | Node(value, left, right) => value + sum(left) + sum(right)
  };

print_int(
  f(
    1111111111111111111111111111111111,
    22222212222222222222222222222222222222,
  ),
);