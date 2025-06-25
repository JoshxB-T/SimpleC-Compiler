//
// Expected: 119 + 1 tokens
//
void main()
{
  int x; // 8
  cin >> x; // 12
  cout << x; // 16
  x = 123*5; // 22
  
  if (true) // 26
    if (false) // 30
      if (x<y) // 36
        if (x>y) // 42
          if (123<=12345) // 48
            if (0>=0) // 54
              if (100!=200) // 60
                if (1==2) // 66
                  x = y; // 70
                else
                  y = 1-2; // 77
              else
                y = x*y; // 84
            else
              count = count/2; // 91
          else
            x=x^y; // 98
        else
          var=1+var; // 105
      else
        y="abc"; // 110
        
  cout << "done";
  cout << endl;
} // 119
