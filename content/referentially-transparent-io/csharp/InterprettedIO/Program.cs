/*
Modified from https://gist.github.com/dadhi/59cfc698f6dc6e31b722cd804aae185a
which was in turn modified from: https://gist.github.com/louthy/524fbe8965d3a2aae1b576cdd8e971e4

Simplified to a program that adds two numbers

Useful links:
- [John DeGoes: Beyond Free Monads - λC Winter Retreat 2017](https://www.youtube.com/watch?v=A-lmrvsUi2Y)
- [Free and tagless compared - how not to commit to a monad too early](https://softwaremill.com/free-tagless-compared-how-not-to-commit-to-monad-too-early)

Requires C# 7.2
*/

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace FreeIOMonadExample
{
    using static Unit;
    using static Operations;

    // We split out our declarative program, which is written using
    // IO operations, from the interpretation of that program.
    //
    // Our program 'recipe' is:
    // * write an introduction to the program
    // * collect the first int
    // * collect the second int
    // * calculate the result (pure)
    // * print out the result
    // * return the result (lifted into IO)
    //
    // The interpretters are then the chefs which take the recipe
    // we have written and _actually_ produce the real world result
    // in all it's side-effecting glory.
    //-------------------------------------------------------------------
    static class Program
    {
        public static int Main()
        {
            // Describe program without running it
            var program = AddTwoNumbers();

	    // Then something else actually runs it
	    return NiceInterpretter.Run(program);
	    //return MeanInterpretter.Run(program);

	    //var testInterpretter = new TestInterpretter();
	    //var result = testInterpretter.Run(program);

	    //Console.WriteLine($"Number of collections: {testInterpretter.NumberOfCollections}");
	    //Console.WriteLine($"Number of log messages: {testInterpretter.LogMessages.Count()}");
	    //return result;
        }

	// Program description
        private static IO<int> AddTwoNumbers() =>
              from _1 in WriteIntroduction()
              from x in CollectIntFromUser()
              from y in CollectIntFromUser()
	      let result = x + y
	      from _2 in WriteResult(result)
              select result;
    }

    // These are the 'instructions' we can use in our program. The
    // inrepretters will then handle these instructions however they
    // see fit.
    //-------------------------------------------------------------------

    public readonly struct WriteIntroduction
    {
    }

    public readonly struct CollectIntFromUser
    {
    }

    public readonly struct WriteResult
    {
	public readonly int Result;
	public WriteResult(int result) => Result = result;
    }

     public readonly struct Log
    {
        public readonly string Message;
        public Log(string message) => Message = message;
    }

    // These are the monadic IO 'operations'. They lift the
    // corresponding instruction into IO.
    //-------------------------------------------------------------------
    public static class Operations
    {
        public static IO<Unit> WriteIntroduction() =>
            new WriteIntroduction().ToIO();

        public static IO<Unit> WriteResult(int result) =>
            new WriteResult(result).ToIO();

        public static IO<int> CollectIntFromUser() =>
            new CollectIntFromUser().ToIO<CollectIntFromUser, int>();

        public static IO<Unit> Log(string message) =>
            new Log(message).ToIO();
    }

   // This is an interpretter with some manners.
   //-------------------------------------------------------------------
   public static class NiceInterpretter
    {
	public static A Run<A>(IO<A> program)
	{
	    switch (program)
            {
                case Return<A> r:
                    return r.Result;
                case IO<WriteIntroduction, Unit, A> x:
                    return Run(x.As(_ => WriteIntroduction()));
                case IO<CollectIntFromUser, int, A> x:
                    return Run(x.As(_ => CollectIntFromUser()));
               	case IO<WriteResult, Unit, A> x:
                    return Run(x.As(i => WriteResult(i.Result)));
                case IO<Log, Unit, A> x:
                    return Run(x.As(i => Console.WriteLine($"LOG: {i.Message}")));

                default: throw new NotSupportedException($"Not supported operation {program}");
            }
	}

	private static Unit WriteIntroduction()
	{
	    Console.WriteLine($"########### {nameof(NiceInterpretter)} #############");
	    WriteNewLine();
            Console.WriteLine(
	        "You look nice today. I'm here to help you add two numbers. " +
	        "All you have to do is enter them :)"
	    );
	    WriteNewLine();
	    return unit;
	}

	private static int CollectIntFromUser()
	{
            Console.Write("Please enter a number: ");
	    return int.Parse(Console.ReadLine());
	}

	private static Unit WriteResult(int result)
	{
	    WriteNewLine();
            Console.WriteLine($"Result is: {result}");
            Console.WriteLine($"You did such a good job. Have a great day!");
	    return unit;
	}

	private static void WriteNewLine() => Console.WriteLine();
    }

   // This is a mean interpretter. Not very supportive at all.
   //-------------------------------------------------------------------
   public static class MeanInterpretter
    {
	public static A Run<A>(IO<A> program)
	{
	    switch (program)
            {
                case Return<A> r:
                    return r.Result;
                case IO<WriteIntroduction, Unit, A> x:
                    return Run(x.As(_ => WriteIntroduction()));
                case IO<CollectIntFromUser, int, A> x:
                    return Run(x.As(_ => CollectIntFromUser()));
               	case IO<WriteResult, Unit, A> x:
                    return Run(x.As(i => WriteResult(i.Result)));
                case IO<Log, Unit, A> x:
                    return Run(x.As(i => Console.WriteLine($"LOG: {i.Message}")));

                default: throw new NotSupportedException($"Not supported operation {program}");
            }
	}

	private static Unit WriteIntroduction()
	{
	    Console.WriteLine($"########### {nameof(MeanInterpretter)} #############");
	    WriteNewLine();
            Console.WriteLine(
	        "I guess I'll some numbers if you give them to me. Also, you smell bad."
	    );
	    WriteNewLine();
	    return unit;
	}

	private static int CollectIntFromUser()
	{
            Console.Write("Give me a number. Now: ");
	    return int.Parse(Console.ReadLine());
	}

	private static Unit WriteResult(int result)
	{
	    WriteNewLine();
            Console.WriteLine($"Result is: {result}... You really couldn't figure that out on your own?");
            Console.WriteLine($"I hate you so much.");
	    return unit;
	}

	private static void WriteNewLine() => Console.WriteLine();
    }

   // Useful for testing maybe?
   //-------------------------------------------------------------------
   public class TestInterpretter
   {
	public int NumberOfCollections { get; private set; }
	public List<string> LogMessages { get; }

	public TestInterpretter()
	{
	    NumberOfCollections = 0;
  	    LogMessages = new List<string>();
	}

	public A Run<A>(IO<A> program)
	{
	    switch (program)
            {
                case Return<A> r:
                    return r.Result;
                case IO<WriteIntroduction, Unit, A> x:
                    return Run(x.As(_ => WriteIntroduction()));
                case IO<CollectIntFromUser, int, A> x:
                    return Run(x.As(_ => CollectIntFromUser()));
               	case IO<WriteResult, Unit, A> x:
                    return Run(x.As(i => WriteResult(i.Result)));
                case IO<Log, Unit, A> x:
                    return Run(x.As(i => {
		        LogMessages.Add(i.Message);
			Console.WriteLine($"LOG: {i.Message}");
		    }));

                default: throw new NotSupportedException($"Not supported operation {program}");
            }
	}

	private static Unit WriteIntroduction()
	{
	    Console.WriteLine($"########### {nameof(TestInterpretter)} #############");
	    WriteNewLine();
	    return unit;
	}

	private int CollectIntFromUser()
	{
	    NumberOfCollections++;
            Console.Write("Please enter a number: ");
	    return int.Parse(Console.ReadLine());
	}

	private static Unit WriteResult(int result)
	{
	    WriteNewLine();
            Console.WriteLine($"Result is: {result}");
	    return unit;
	}

	private static void WriteNewLine() => Console.WriteLine();
    }


    // Monadic IO implementation, can be reused, published to NuGet, etc.
    //-------------------------------------------------------------------

    public interface IO<A>
    {
        IO<B> Bind<B>(Func<A, IO<B>> f);
    }

    public sealed class Return<A> : IO<A>
    {
        public readonly A Result;
        public Return(A a) => Result = a;

        public IO<B> Bind<B>(Func<A, IO<B>> f) => f(Result);
    }

    public class IO<I, O, A> : IO<A>
    {
        public readonly I Input;
        public readonly Func<O, IO<A>> Next;
        public IO(I input, Func<O, IO<A>> next) => (Input, Next) = (input, next);

        public IO<B> Bind<B>(Func<A, IO<B>> f) => new IO<I, O, B>(Input, r => Next(r).Bind(f));
    }

    public static class IOMonad
    {
        public static IO<A> Lift<A>(this A a) =>
            new Return<A>(a);

        public static IO<B> Select<A, B>(this IO<A> m, Func<A, B> f) =>
	    m.Bind(a => f(a).Lift());

        public static IO<C> SelectMany<A, B, C>(this IO<A> m, Func<A, IO<B>> f, Func<A, B, C> project) =>
            m.Bind(a => f(a).Bind(b => project(a, b).Lift()));
    }

    public static class IOMonadSugar
    {
        public static IO<R> ToIO<I, R>(this I input) => new IO<I, R, R>(input, IOMonad.Lift);
        public static IO<Unit> ToIO<I>(this I input) => input.ToIO<I, Unit>();

        public static IO<A> Ignore<I, A>(this IO<I, Unit, A> x) => x.Next(unit);

        public static IO<A> As<I, O, A>(this IO<I, O, A> x, Func<I, O> process) => x.Next(process(x.Input));
        public static IO<A> As<I, A>(this IO<I, Unit, A> x, Action<I> process)
        {
            process(x.Input);
            return x.Ignore();
        }
    }

    public struct Unit
    {
        public static readonly Unit unit = new Unit();
    }
}
