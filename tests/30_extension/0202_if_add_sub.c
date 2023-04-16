{
  int x;
  int y;
  int z;
  x = rand(10,20);
  y = rand(0,10);
  if(y>5){ x = y - x; }
  else { x = y + x; }
  z = 100/x;
  print_all;
}