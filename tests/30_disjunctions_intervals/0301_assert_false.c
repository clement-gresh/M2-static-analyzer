{
  int x;
  x = rand(0,10);
  if(x < 2) { x = x -10; }
  assert (x < 5);
  print(x);
}
