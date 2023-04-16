{
  int x;
  x = rand(0,15);
  if(x < 10){
    while (x < 10) {
      print(x);
      x = x - 1;
    }
  }
  print(x);
}

