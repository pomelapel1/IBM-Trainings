       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.
      *--------------------------------------------------------------*
      * Workshop 15 -  Finish coding the subroutines from the
      *     Employee's Projects File based on the descriptions
      *     written on the subroutine.
      *
      ***  Subroutine 400-TOTAL-PROJ-EXPENSE requiremenent is below.
      ***  Calculate and Display the total cost for the 'A111' project
      ***  Also included in this subroutine the total costs for all
      ***  of the other projects. A good way of cost comparison
      ***  among projects.
      ***
      ***  Requirements for subroutine 500-TOTAL-ALL-PROJECTS-EXPENSE
      ***  Calculate & Display the total cost for all of the projects
      ***   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL)).
      ***
      ***  Just for the purpose of using the SUM INTRINSIC function,
      ***  as suggested in 15.2, they are being used in this subroutine.
      ***
      ***  I, however, use the MEAN function for the rates to get a
      ***  closer value to the accurate, total cost of the Project.
      ***  To get the accurate 'Total Cost of the Project',
      ***  I used the subtotals for each projects from
      ***  400-TOTAL-PROJ-EXPENSE.
      ***
      ***  Subroutine 600-FIND-PA-FROM-NC-A111, counts how
      ***  many Programmer/Analyst work on Project 'A111'
      ***  and are from North Carolina.   This subroutine also will
      ***  showcase how to make a non case-sensitive search by
      ***  converting one operand to upper-case.
      ***
      ***  Plus :  Bonus Optional questions for 15.5.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPPROJ.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  EMP-PROJECT-TABLE-I.
           05 EMP-PROJECT-I                 PIC X(4).
           05 EMP-NAME-I                    PIC X(15).
           05 EMP-STATE-OFFICE-I            PIC X(02).
           05 EMP-PROJECT-POSITION-I        PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ-I        PIC 9(03).
           05 EMP-NBR-OT-HOURS-I            PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE-I    PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE-I        PIC 9(03)V99.
           05 EMP-LANGUAGE-CERT-I           PIC X(20).
           05 EMP-ON-CALL-I                 PIC X(01).
           05 FILLER                        PIC X(02).
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX                    PIC S9(4) COMP.
       77  TABLE-MAX                        PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE                   PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.
       77  SUM-1                            PIC 9(18) VALUE 0.
       77  MAX-OUT                          PIC 9(4).
      *
       01  TABLE-TOTALS.
           05 A111-TOT                      PIC S9(7)V99 COMP.
           05 A333-TOT                      PIC S9(7)V99 COMP.
           05 B222-TOT                      PIC S9(7)V99 COMP.
           05 B333-TOT                      PIC S9(7)V99 COMP.
           05 C333-TOT                      PIC S9(7)V99 COMP.
           05 D444-TOT                      PIC S9(7)V99 COMP.
           05 TBL-TOT-NBR-DYS-ON-PROJ       PIC S9(6)V99 COMP.
           05 TBL-AVG-PER-DAY-BILL-RATE     PIC S9(6)V99 COMP.
           05 TBL-TOT-NBR-OT-HRS            PIC S9(6)V99 COMP.
           05 TBL-AVG-PER-HOUR-OT-RATE      PIC S9(6)V99 COMP.
           05 TBL-TOT-PROJ-COST             PIC S9(9)V99 COMP.
           05 A111-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 A333-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 B222-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 B333-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 C333-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 D444-TOT-OUT                  PIC $$$,$$$,$$$.99.
           05 TBL-TOT-NBR-DYS-ON-PROJ-OUT   PIC $$,$$$,$$$.99.
           05 TBL-AVG-PER-DAY-BILL-RATE-OUT PIC $$,$$$,$$$.99.
           05 TBL-TOT-NBR-OT-HRS-OUT        PIC $$,$$$,$$$.99.
           05 TBL-AVG-PER-HOUR-OT-RATE-OUT  PIC $$,$$$,$$$.99.
           05 TBL-TOT-PROJ-COST-OUT         PIC $,$$$,$$$,$$$.99.
      *
           05 HIGH-PAID-PA-SALARY           PIC S9(3)V99 COMP.
           05 HIGH-PAID-PA-NAME             PIC X(15) VALUE SPACES.
           05 HIGH-PAID-PA-SALARY-OUT       PIC $,$$$.99.
           05 FOUND-CTR                     PIC 99 VALUE ZEROES.
           05 FOUND-SWITCH                  PIC X  VALUE 'N'.
      *
       01  EMP-PROJECT-TABLE.
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES
                INDEXED BY PROJ-IDX.
                10 EMP-PROJECT               PIC X(4).
                10 EMP-NAME                  PIC X(15).
                10 EMP-STATE-OFFICE          PIC X(02).
                10 EMP-PROJECT-POSITION      PIC X(20).
                10 EMP-NBR-DAYS-ON-PROJ      PIC 9(03).
                10 EMP-NBR-OT-HOURS          PIC 9(03).
                10 EMP-PER-DAY-BILLING-RATE  PIC 9(03)V99.
                10 EMP-PER-HOUR-OT-RATE      PIC 9(03)V99.
                10 EMP-LANGUAGE-CERT         PIC X(20).
                10 EMP-ON-CALL               PIC X(01).
                10 FILLER                    PIC X(02).
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE EMP-PROJECT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
                 AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL PROJECT-INDEX > TABLE-MAX              *> Load Table
      *             OR END-OF-FILE
                MOVE EMP-PROJECT-I TO
                        EMP-PROJECT (PROJECT-INDEX)
                MOVE EMP-NAME-I TO
                        EMP-NAME (PROJECT-INDEX)
                MOVE EMP-STATE-OFFICE-I TO
                        EMP-STATE-OFFICE  (PROJECT-INDEX)
                MOVE EMP-PROJECT-POSITION-I TO
                        EMP-PROJECT-POSITION  (PROJECT-INDEX)
                MOVE EMP-NBR-DAYS-ON-PROJ-I TO
                        EMP-NBR-DAYS-ON-PROJ (PROJECT-INDEX)
                MOVE EMP-NBR-OT-HOURS-I  TO
                        EMP-NBR-OT-HOURS (PROJECT-INDEX)
                MOVE EMP-PER-DAY-BILLING-RATE-I TO
                        EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX)
                MOVE EMP-PER-HOUR-OT-RATE-I  TO
                        EMP-PER-HOUR-OT-RATE (PROJECT-INDEX)
                MOVE EMP-LANGUAGE-CERT-I  TO
                        EMP-LANGUAGE-CERT (PROJECT-INDEX)
                MOVE EMP-ON-CALL-I   TO
                        EMP-ON-CALL (PROJECT-INDEX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
                DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
           END-PERFORM.
       100-PROCESS-TABLE-DATA.
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.
           PERFORM 500-TOTAL-ALL-PROJECTS-EXPENSE.
           PERFORM 600-FIND-PA-FROM-NC-A111.
           PERFORM 650-HIGHEST-PAID-PA.
           PERFORM 700-DBA-FROM-C222.
           PERFORM 750-EMPL-FROM-333S.
           PERFORM 800-FIND-KANDACE.
      *
       200-FIND-PROJECT.
      ***  Display all of the Employee names working on project 'A111'
           DISPLAY "  ".
           DISPLAY "EMPLOYEES WORKING ON PROJECT A111".
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT(PROJ-IDX) = 'A111'
                DISPLAY EMP-NAME(PROJ-IDX) "     "
                          EMP-PROJECT-POSITION(PROJ-IDX)
             END-IF
           END-PERFORM.
      *
       300-FIND-NC-OT-SKILL.
      ***  Display all of the Employee names of Programmers in NC
      ***     who are allowed to bill for On-Call work
           DISPLAY " ".
           DISPLAY "NORTH CAROLINA EMPLOYEES ALLOWED FOR OVERTIME".
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
            IF EMP-STATE-OFFICE(PROJ-IDX) = 'NC' AND
               EMP-ON-CALL(PROJ-IDX) = 'Y'
               DISPLAY EMP-NAME(PROJ-IDX)  "     "
                        EMP-PROJECT-POSITION(PROJ-IDX)

             END-IF
           END-PERFORM.
      *
       400-TOTAL-PROJ-EXPENSE.
      ***  Calculate and Display the total cost for the 'A111' project
      *
           MOVE ZEROES TO A111-TOT A333-TOT B222-TOT B333-TOT
                          C333-TOT D444-TOT.
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT(PROJ-IDX) = 'A111'
                COMPUTE A111-TOT = (A111-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
      ***  Calculate and Display the total cost for the 'A333' project
             IF EMP-PROJECT(PROJ-IDX) = 'A333'
                COMPUTE A333-TOT = (A333-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
      ***  Calculate and Display the total cost for the 'B222' project
             IF EMP-PROJECT(PROJ-IDX) = 'B222'
                COMPUTE B222-TOT = (B222-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
      ***  Calculate and Display the total cost for the 'B333' project
             IF EMP-PROJECT(PROJ-IDX) = 'B333'
                COMPUTE B333-TOT = (B333-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
      ***  Calculate and Display the total cost for the 'C333' project
             IF EMP-PROJECT(PROJ-IDX) = 'C333'
                COMPUTE C333-TOT = (C333-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
      ***  Calculate and Display the total cost for the 'D444' project
             IF EMP-PROJECT(PROJ-IDX) = 'D444'
                COMPUTE D444-TOT = (D444-TOT) +
                        (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX)) +
                        (EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
             END-IF
      *
           END-PERFORM.
           MOVE    A111-TOT TO A111-TOT-OUT.
           MOVE    A333-TOT TO A333-TOT-OUT.
           MOVE    B222-TOT TO B222-TOT-OUT.
           MOVE    B333-TOT TO B333-TOT-OUT.
           MOVE    C333-TOT TO C333-TOT-OUT.
           MOVE    D444-TOT TO D444-TOT-OUT.
           DISPLAY " ".
           DISPLAY 'TOTAL A111 PROJECT COST ' A111-TOT-OUT.
           DISPLAY 'TOTAL A333 PROJECT COST ' A333-TOT-OUT.
           DISPLAY 'TOTAL B222 PROJECT COST ' B222-TOT-OUT.
           DISPLAY 'TOTAL B333 PROJECT COST ' B333-TOT-OUT.
           DISPLAY 'TOTAL C333 PROJECT COST ' C333-TOT-OUT.
           DISPLAY 'TOTAL D444 PROJECT COST ' D444-TOT-OUT.
           COMPUTE TBL-TOT-PROJ-COST = A111-TOT + A333-TOT +
                   B222-TOT + B333-TOT + C333-TOT + D444-TOT
           MOVE TBL-TOT-PROJ-COST  TO TBL-TOT-PROJ-COST-OUT.
           DISPLAY "  ".
           DISPLAY "ACTUAL TOTAL COST FOR ALL OF THE PROJECTS : "
                    TBL-TOT-PROJ-COST-OUT.
      *
       500-TOTAL-ALL-PROJECTS-EXPENSE.
      ***  Calculate & Display the total cost for all of the projects
      **   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL))
      *
           MOVE ZEROES TO TBL-TOT-PROJ-COST.
           COMPUTE TBL-TOT-NBR-DYS-ON-PROJ =
                   FUNCTION SUM(EMP-NBR-DAYS-ON-PROJ(ALL)).
           COMPUTE TBL-AVG-PER-DAY-BILL-RATE =
                   FUNCTION MEAN(EMP-PER-DAY-BILLING-RATE(ALL)).
           COMPUTE TBL-TOT-NBR-OT-HRS =
                   FUNCTION SUM(EMP-NBR-OT-HOURS(ALL)).
           COMPUTE TBL-AVG-PER-HOUR-OT-RATE =
                   FUNCTION MEAN(EMP-PER-HOUR-OT-RATE(ALL)).
      *
           COMPUTE TBL-TOT-PROJ-COST =
              (TBL-TOT-NBR-DYS-ON-PROJ * TBL-AVG-PER-DAY-BILL-RATE) +
              (TBL-TOT-NBR-OT-HRS * TBL-AVG-PER-HOUR-OT-RATE).
      *
           MOVE TBL-TOT-NBR-DYS-ON-PROJ    TO
                        TBL-TOT-NBR-DYS-ON-PROJ-OUT.
           MOVE TBL-AVG-PER-DAY-BILL-RATE  TO
                        TBL-AVG-PER-DAY-BILL-RATE-OUT.
           MOVE TBL-TOT-NBR-OT-HRS         TO TBL-TOT-NBR-OT-HRS-OUT.
           MOVE TBL-AVG-PER-HOUR-OT-RATE   TO
                        TBL-AVG-PER-HOUR-OT-RATE-OUT.
           MOVE TBL-TOT-PROJ-COST  TO TBL-TOT-PROJ-COST-OUT.
      *
           DISPLAY "  ".
           DISPLAY "  ".
           DISPLAY "TOTAL # DAYS WORKED ON THE PROJECT            : "
                                    TBL-TOT-NBR-DYS-ON-PROJ-OUT.
           DISPLAY "AVERAGE OF EMPLOYEES' DAILY RATES ON PROJECT  : "
                                    TBL-AVG-PER-DAY-BILL-RATE-OUT.
           DISPLAY "TOTAL # OF OT HRS WORKED ON THE PROJECT       : "
                                    TBL-TOT-NBR-OT-HRS-OUT.
           DISPLAY "AVERAGE OF EMPLOYEES' OT RATES ON PROJECT     : "
                                    TBL-AVG-PER-HOUR-OT-RATE-OUT.
           DISPLAY "ESTIMATED TOTAL COST FOR ALL OF THE PROJECTS : "
                    TBL-TOT-PROJ-COST-OUT.
      *
       600-FIND-PA-FROM-NC-A111.
      ***  Count how many Programmer/Analyst work on Project 'A111'
      ***  and are from North Carolina.
      ***
           MOVE ZEROES TO FOUND-CTR.
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT(PROJ-IDX) = 'A111' AND
                FUNCTION UPPER-CASE(EMP-PROJECT-POSITION(PROJ-IDX))
                    = 'PROGRAMMER/ANALYST'
                ADD 1 TO FOUND-CTR
             END-IF
           END-PERFORM.
           DISPLAY " ".
           DISPLAY 'TOTAL # OF PROGRAMMER/ANALYST ON PROJECT A111 '
                    FOUND-CTR.
      *
       650-HIGHEST-PAID-PA.
      *
           INITIALIZE HIGH-PAID-PA-SALARY HIGH-PAID-PA-NAME.
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT-POSITION(PROJ-IDX) = 'PROGRAMMER/ANALYST'
                AND EMP-PER-DAY-BILLING-RATE(PROJ-IDX) >
                    HIGH-PAID-PA-SALARY
                    MOVE EMP-NAME(PROJ-IDX) TO  HIGH-PAID-PA-NAME
                    MOVE EMP-PER-DAY-BILLING-RATE(PROJ-IDX) TO
                                                HIGH-PAID-PA-SALARY
             END-IF
           END-PERFORM.
           DISPLAY " ".
           DISPLAY 'HIGHEST PAID PROGRAMMER ANALYST ' HIGH-PAID-PA-NAME.
           MOVE    HIGH-PAID-PA-SALARY TO HIGH-PAID-PA-SALARY-OUT.
           DISPLAY 'WITH A DAILY BILLING RATE OF '
                    HIGH-PAID-PA-SALARY-OUT.
      *
       700-DBA-FROM-C222.
      *** Find the DBA for project C222.  But there is no project
      *** 'C222'.
      ***
           DISPLAY " ".
           DISPLAY 'DATABASE ADMIN FOR C222'.
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT(PROJ-IDX) = 'C222' AND
                EMP-PROJECT-POSITION(PROJ-IDX)= 'DATABASE ADMIN'
                DISPLAY EMP-NAME(PROJ-IDX) "   "
                        EMP-PROJECT-POSITION(PROJ-IDX)
                MOVE 'Y' TO FOUND-SWITCH
             END-IF
           END-PERFORM.
           IF FOUND-SWITCH NOT = 'Y'
              DISPLAY 'NO RECORD FOUND-DATABASE ADMIN FOR PROJECT C222'
           END-IF.
      ***
       750-EMPL-FROM-333S.
      ***
      *** List all employees who work on project ending -333.
      *** i.e. A333, B333, C333 & etc.
      ***
           DISPLAY " ".
           DISPLAY 'EMPLOYEES ON PROJECTS ENDING WITH -333 '.
           SET  PROJ-IDX UP BY 1.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1  UNTIL
                    PROJ-IDX > TABLE-MAX
             IF EMP-PROJECT(PROJ-IDX)(2:3) = '333'
                DISPLAY EMP-PROJECT(PROJ-IDX) "    "
                        EMP-NAME(PROJ-IDX) "    "
                        EMP-PROJECT-POSITION(PROJ-IDX)
             END-IF
           END-PERFORM.
      ***
       800-FIND-KANDACE.
           SET PROJ-IDX UP BY 1.
           MOVE SPACE TO FOUND-SWITCH.
           DISPLAY " ".
           DISPLAY "DOES KANDACE SPRINGS PROGRAM COBOL ? ".
           PERFORM VARYING PROJ-IDX FROM 1 BY 1 UNTIL
               PROJ-IDX > TABLE-MAX OR FOUND-SWITCH = 'Y'
               IF 'KANDACE SPRINGS' = EMP-NAME(PROJ-IDX)
                  MOVE 'Y' TO FOUND-SWITCH
                  IF EMP-LANGUAGE-CERT(PROJ-IDX)(1:5) = 'COBOL'
                     DISPLAY 'YES, KANDACE SPRINGS PROGRAMS COBOL'
                  ELSE
                     DISPLAY 'NO, KANDACE SPRINGS DO NOT PROGRAM COBOL'
                  END-IF
               END-IF
           END-PERFORM.
           IF FOUND-SWITCH = 'N'
              DISPLAY "EMPLOYEE KANDACE SPRINGS IS NOT FOUND !"
           END-IF.
      ***
       900-WRAP-UP.
           CLOSE INPUT-FILE.