//
// Testing expectations
// --------------------
// tokens: 21
// parser: "syntax error: expecting ;, but found indentifier:x"
//
void main()
{
    int x     // Forgot semicolon. Oops!

    x = 0;
    cout << x;
    cout << endl;
}
