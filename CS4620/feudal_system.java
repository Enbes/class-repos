/****************************************************************/
/* GradeBook Application Main class (Section 5.6)               */
/* Needs grade1.java and grade2.java to be compiled             */
/* Chapter 5; Oracle Programming -- A Primer                    */
/*            by R. Sunderraman                                 */
/****************************************************************/

import java.io.*; 
import java.sql.*;

class feudal_system { 

  public static void main (String args []) 
      throws SQLException, IOException { 

    feudal f = new feudal();
    boolean done;
    char ch,ch1;
    byte num = 0;

    try {
      Class.forName ("oracle.jdbc.driver.OracleDriver");
    } catch (ClassNotFoundException e) {
        System.out.println ("Could not load the driver");
        return;
      }
    String user, pass;
    user = readEntry("userid  : ");
    pass = readEntry("password: ");

    //  The following line was modified by Prof. Marling to work on prime

    Connection conn = DriverManager.getConnection
       ("jdbc:oracle:thin:@deuce.cs.ohiou.edu:1521:class", user, pass);

    done = false;
    do {
      f.print_menu();
      System.out.print("Type in your option:");
      System.out.flush();
      ch = (char) System.in.read();
      ch1 = (char) System.in.read();
      switch (ch) {
        case '1' : f.view_nobles(conn);
                    break;
        case '2' : f.add_noble(conn);
                   break;
        case '3' : f.delete_noble(conn);
                   break;
        case '4' : f.modify_noble(conn);
                   break;
        case '5' : f.useful1(conn);
                   break;
        case '6' : f.useful2(conn);
                   break;
        case 'q' : done = true;
                   break;
        default  : System.out.println("Type in option again");
      }
    } while (!done);

    conn.close();
  }

  //readEntry function -- to read input string
  static String readEntry(String prompt) {
     try {
       StringBuffer buffer = new StringBuffer();
       System.out.print(prompt);
       System.out.flush();
       int c = System.in.read();
       while(c != '\n' && c != -1) {
         buffer.append((char)c);
         c = System.in.read();
       }
       return buffer.toString().trim();
     } catch (IOException e) {
       return "";
       }
   }
} 