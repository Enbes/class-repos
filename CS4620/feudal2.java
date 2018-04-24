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
    System.out.println("(3) Remove Noble");
    System.out.println("(4) Modify Numerical Noble Attribute");
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

  void view_nobles(Connection conn)
  throws SQLException, IOException {
    String query = "select * from noble";
    Statement stmt = conn.createStatement();
    ResultSet rset;

    try {
      rset = stmt.executeQuery(query);
    } 
    catch (SQLException e) {
        System.out.println("Problem reading table");
        while (e != null) {
          System.out.println("Message     : " + e.getMessage());
          e = e.getNextException();
        }
        return;
    }

    System.out.print("NNAME PTNAME DNAME SEX BDATE WEALTH LEVY DEMESNE RUNAME RUPTNAME RRELATION");

    while (rset.next()) {
      System.out.print(rset.getstring(1));
    }
  }

  void asnt_noble(Connection conn)
  throws SQLException, IOException {
    String nname = readEntry("Name of noble to remove: ");
    String ptname = readEntry("Primary title of noble to remove: ");
    String query = "delete noble where nname = '" + nname +
                   "' and ptname = '" + ptname + "'";

    conn.setAutoCommit(false);
    Statement stmt = conn.createStatement (); 

    int res;
    try {
      res = stmt.executeUpdate(query);
    }
    catch (SQLException e) {
        System.out.println("Could not remove noble");
        while (e != null) {
          System.out.println("Message     : " + e.getMessage());
          e = e.getNextException();
        }
        conn.rollback();
        return;
    }
    System.out.println("Removed noble");
    conn.commit();
    conn.setAutoCommit(true);
    stmt.close();    
  }

  void modify_noble(Connection conn)
  throws SQLException, IOException {
    String nname    = readEntry("Noble's name: ");
    String ptname = readEntry("Noble's primary title: ");
    String att = readEntry("Numerical attribute to modify (levy/wealth/demesne): ");
    String query1 = "select " + att + "from noble where nname = '" + nname + "' and ptname = '" + ptname + "'";

    Statement stmt = conn.createStatement (); 
    ResultSet rset;
    try {
      rset = stmt.executeQuery(query1);
    } catch (SQLException e) {
        System.out.println("Error");
        while (e != null) {
          System.out.println("Message     : " + e.getMessage());
          e = e.getNextException();
        }
        return;
      }
    System.out.println("");
    if ( rset.next ()  ) {
      System.out.println("Old " + att + " = " + rset.getString(1));
      String na = readEntry("Enter new " + att + ": ");
      String query2 = "update noble set " + att " = " + na + 
                      " where nname = '" + nname + "' and ptname = '" +
                      ptname + "'";
      try {
        stmt.executeUpdate(query2);
      } 
      catch (SQLException e) {
        System.out.println("Could not modify " + att);
        while (e != null) {
          System.out.println("Message     : " + e.getMessage());
          e = e.getNextException();
        }
        return;
      }
      System.out.println("Modified " + att + " successfully");
    }
    else 
      System.out.println(att + " not found");
    stmt.close();    
  }

  void re_rep(Connection conn)
  throws SQLException, IOException {
  	String realmname = readEntry("Realm to analyze: ");

  	String query1 = "select * from realm where realmname = '" + realmname + "'";
  	Statement stmt1 = conn.createStatement();
  	ResultSet rset1;
  	try {
  		rset1 = stmt1.executeQuery(query1);
  	}
  	catch (SQLException e) {
  		System.out.println("Error reading table");
  		while (e != null) {
  			System.out.println("Message 	: " + e.getMessage());
  			e = e.getNextException();
  		}
  		return;
  	}
    System.out.printf("%-22s%-22s%-22s%-22s\n","REALMNAME","C_DATE","RULERNAME","RUPTNAME");
    while (rset.next()) {
      System.out.printf("%-22s%-22s%-22s%-22s\n\n",rset1.getString(1),rset1.getString(2),rset1.getString(3),rset1.getString(4));
    }

    String query2 = "select * from realm, noble where realmname = '" + realmname + "' and realm.runame = noble.runame and realm.rptname = noble.rptname";
    Statement stmt2 = conn.createStatement();
    ResultSet rset2;
    try {
    	rset2 = stmt2.executeQuery(query2);
    }
  	catch (SQLException e) {
  		System.out.println("Error reading table");
  		while (e != null) {
  			System.out.println("Message 	: " + e.getMessage());
  			e = e.getNextException();
  		}
  		return;
  	}
  	System.out.println("Nobles who live in this realm:");
  	System.out.printf("%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s\n",
  					  "NNAME", "PTNAME", "DNAME", "SEX", "BDATE", "WEALTH", "LEVY", "DEMESNE", "RUNAME", "RUPTNAME", "RRELATION");
  	System.out.printf("%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s%-22s\n\n",
  					  rset2.getString(1),rset2.getString(2),rset2.getString(3),rset2.getString(4),rset2.getString(5),
  					  rset2.getString(6),rset2.getString(7),rset2.getString(8),rset2.getString(9),rset2.getString(10),rset2.getString(11));

  	System.out.println("Realm statistics:");
  	//total wealth, levy, and demesne - also total # of nobles and # of territories (as well as the territories themselves)
  	//don't forget to add query to display selected dynasty in other report, also add dynasty statistics
  }