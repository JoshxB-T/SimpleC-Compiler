//
// Testing expectations
// --------------------
// tokens: 21
// compilation: "syntax error: expecting ;, but found indentifier:x"
//
void main()
{
    int x     // syntax error

    x = 0;
    cout << x;
    cout << endl;
}
