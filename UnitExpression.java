import java.util.*;
import java.math.*;
public class UnitExpression {
	/*
	 making the max array long enough to handle any input (hopefully)
	 */
	public static int MAX_ARRAY_LENGTH = 10000;
	private static int countArray[] = new int[MAX_ARRAY_LENGTH];
	private static boolean init = false;
	private static void initArray()
	{
	for (int j=1; j <= 5; j++)
	{ /*
	these values cannot be expressed in any units smaller than the number itself
	*/
		countArray[j] = j;}

	for (int i = 6; i < MAX_ARRAY_LENGTH; i++)
	{ /*
	setting every value in the array to be 5000 so that it is bigger than any of the possible unit expressions
	*/
		countArray[i] =5000;}
	}
	public static int minLength(int n)
  {
	  if (init == false)
	  {
	  initArray();
	  init = true;
	  }
if (n<=0)   return -1;
  if (n <= 5)
  {
	  /*
	   returning unit expression for trivial cases
	   */
  return n;
  }
  /*
   checking if the minimum value has already been calculated to minimize the computation per recursive call
   */
  if ( countArray[n] < 5000)  
  return countArray[n];        

  int runningCount = 0 ;
      for (int i = 2; i < n; i++)
      {
      if (n%i==0)
      {
    	  /*
    	   checking whether the unit expression could be more quickly reached by 1+n-1 or i*n/i and storing this minimum result in the array
    	   */
    	  runningCount=Math.min(minLength(i) + minLength(n/i), 1 + minLength(n-1));
    	  if (runningCount<countArray[n])
    		  countArray[n] = runningCount;
        }
      }
      /*
       handling prime numbers that made it through the for loop without any divisors being found by subtracting 1
       */
  runningCount=0;
  runningCount += 1+minLength(n-1);
  //System.out.println(runningCount);
                
  if (runningCount<countArray[n]) {
  countArray[n] = runningCount;
  }
  return countArray[n];
}
    public static void main (String[] args) {
        System.out.println(minLength(138));
    }
}