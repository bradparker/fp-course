using System;

namespace Impure.IO
{
    class Program
    {
	static int PureAdd(int x, int y) => x + y; 

	static Func<int> ImpureAdd(int x, int y) => () =>
        {
	    Console.WriteLine("I'm going to add now");
	    return x + y;
	};

        static void Main(string[] args)
        {
	    Func<int> thunk = () =>
            {
	        Console.WriteLine("I'm going to add now");
	        return 2 + 3;
	    };
	    Console.WriteLine($"2 + 3 = {thunk()}");
        }
    }
}
