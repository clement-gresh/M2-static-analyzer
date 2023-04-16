{
  int x;
  x = rand(0,10);
  if(x < 2) {
    x = 0;
  } else {
    if(x < 5){ x = 1 ;}
    else { x = 2; }
  }
  assert ((x <= 1 && ((x==0) || (x==1))) || x==2);
  print(x);
}
