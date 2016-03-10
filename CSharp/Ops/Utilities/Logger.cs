namespace Ops 
{
    public class Logger
    {
        private static ExceptionLogger exceptionLogger = new ExceptionLogger();

        public static ExceptionLogger ExceptionLogger 
        { 
            get 
            { 
                return exceptionLogger; 
            } 
        }
    }
}
