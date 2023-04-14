{
  int x;
  int y;
  int z;
  x = rand(10,20);
  y = rand(0,1);
  if(y>0){ x = -x; }
  z = 100/x;
  print_all;
}