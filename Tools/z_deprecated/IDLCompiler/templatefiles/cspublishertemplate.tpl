
using OPS;
namespace __packageName
{
    public class __classNamePublisher 
        : Publisher 
    {


        public __classNamePublisher(Topic t)
            :base(t)
        {
            
            setObjectHelper(new __classNameHelper());

        }
        public void Write(__className o) 
        {
            base.Write(o);
        }


    }
}