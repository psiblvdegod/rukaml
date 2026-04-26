int five()
{
    int x = 3;
    x = x + 2;
    return x;
}

int seven()
{
    int x = 4;
    int z = x + 1;
    return z + 2;
}

int main()
{
    int f = five();
    int s = seven();
    return f + s;
}
