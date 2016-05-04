int x = rand(0,100);

void main()
{
  int i;
  for (i=0;i<10;i++) {
    x--;
    if (x<=0) break;
  }
}
