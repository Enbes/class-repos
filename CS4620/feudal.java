/****************************************************************/
/* GradeBook Application: grade1.java (Section 5.6)             */
/* Needs grade2.java to be compiled                             */
/* Chapter 5; Oracle Programming -- A Primer                    */
/*            by R. Sunderraman                                 */
/****************************************************************/

import java.sql.*; 
import java.io.*; 

class feudal { 

  void print_menu() {
    System.out.println("      FEUDAL SYSTEM\n");
    System.out.println("(1) View Nobles");
    System.out.println("(2) Add Noble");
    System.out.println("(3) Delete Noble");
    System.out.println("(4) Modify Noble");
    System.out.println("(5) Something Useful");
    System.out.println("(6) Another Useful Thing");
    System.out.println("(q) Quit\n");
  }

  void add_noble(Connection conn) 
  throws SQLException, IOException {

    Statement stmt = conn.createStatement();

    String nname = readEntry("Noble Name: ");
    String ptname = readEntry("Primary Title: ");
    String dname = readEntry("Dynastic Name: ");
    char sex = readEntry("Sex: ");
    int bdate = ("Birth Year: ");
    int wealth = ("Wealth Amount: ");
    int levy = ("Levy Size: ");
    int demesne = ("Demesne Size: ");
    String runame = ("Ruler's Name: ");
    String ruptname = ("Ruler's Primary Title: ");
    String rrelation = ("Relation to Ruler: ");

    try {
      int nrows = stmt.executeUpdate(query);
    } 
    catch (SQLException e) {
      System.out.println("Error Adding Noble");
      while (e != null) {
        System.out.println("Message     : " + e.getMessage());
        e = e.getNextException();
      }
      return;
    }
    stmt.close();
    System.out.println("Added Noble");
  }

  void select_course(Connection conn) 
    throws SQLException, IOException {

    String query1 = "select distinct lineno,courses.cno,ctitle " +
                    "from courses,catalog " +
                    "where courses.cno = catalog.cno and term = '";
    String query;
    String term_in = readEntry("Term: ");
    query = query1 + term_in + "'";
     
    Statement stmt = conn.createStatement (); 
    ResultSet rset = stmt.executeQuery(query);
    System.out.println("");
    while (rset.next ()) { 
      System.out.println(rset.getString(1) + "   " +
                         rset.getString(2) + "   " +
                         rset.getString(3));
    } 
    System.out.println("");
    String ls = readEntry("Select a course line number: ");
    
    grade2 g2 = new grade2();
    boolean done;
    char ch,ch1;

    done = false;
    do {
      g2.print_menu();
      System.out.print("Type in your option:");
      System.out.flush();
      ch = (char) System.in.read();
      ch1 = (char) System.in.read();
      switch (ch) {
        case '1' : g2.add_enrolls(conn,term_in,ls);
                   break;
        case '2' : g2.add_course_component(conn,term_in,ls);
                   break;
        case '3' : g2.add_scores(conn,term_in,ls);
                   break;
        case '4' : g2.modify_score(conn,term_in,ls);
                   break;
        case '5' : g2.drop_student(conn,term_in,ls);
                   break;
        case '6' : g2.print_report(conn,term_in,ls);
                   break;
        case 'q' : done = true;
                   break;
        default  : System.out.println("Type in option again");
      }
    } while (!done);

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
