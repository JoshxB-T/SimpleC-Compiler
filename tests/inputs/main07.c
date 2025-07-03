//
// Testing expectations
// --------------------
// tokens: 131
// parser: "Success!"
//
void main()
{
  int x;
  int var;
  cin >> x;
  cout << x;
  x = 123*5;
  int y = 10;
  int count;
  
  if (true)
    if (false)
      if (x<y)
        if (x>y)
          if (123<=12345)
            if (0>=0)
              if (100!=200)
                if (1==2)
                  x = y;
                else
                  y = 1-2;
              else
                y = x*y;
            else
              count = count/2;
          else
            x=x^y;
        else
          var=1+var;
      else
        y="abc";

  cout << "done";
  cout << endl;
}
