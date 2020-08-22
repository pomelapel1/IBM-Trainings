       IDENTIFICATION DIVISION.
       PROGRAM-ID. MILLARD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PERSONNEL-REC.
           05  NAME             PIC X(20) .
           05  ADDR             PIC X(40).
           05  DATE-WS          PIC X(30).
           05  RATE             PIC 9(3)V99.
           05  BONUS-RATE       PIC V99.
           05  HOURS            PIC 9(3).
           05  GROSS-PAY        PIC 9(6)V99.
           05  JOB              PIC X(14).
       77  VP                   PIC X(20) VALUE "Abigail Fillmore".
       01  PRSNL-NUM-OUTREC.
           05  RATE-OUT         PIC $$$$$$$.99.
           05  BONUS-RATE-OUT   PIC $$$$$$9.99.
           05  HOURS-OUT        PIC ZZZZZZZZZ9.
           05  GROSS-PAY-OUT    PIC $$$,$$$.99.
       PROCEDURE DIVISION.
           PERFORM PAYROLL-HEADING.
           PERFORM ASSIGNMENT-PARAGRAPH.
           PERFORM CONDITIONAL-SELECTION.
           PERFORM DISPLAY-DATA-PARAGRAPH.
           PERFORM ASSIGN-VP.
           PERFORM CONDITIONAL-SELECTION.
           PERFORM DISPLAY-DATA-PARAGRAPH.
           GOBACK.
      *
      ****** DISPLAY PAYROLL HEADING
      *
       PAYROLL-HEADING.
           MOVE  "Week of February 24th, 2020" TO DATE-WS.
           DISPLAY "Today's Date    : " DATE-WS.
           DISPLAY "  ".
           DISPLAY "  ".
      ****** COBOL MOVE statements - Literals assigned to variables
       ASSIGNMENT-PARAGRAPH.
           MOVE  "Millard Fillmore"                     TO NAME.
           MOVE "61 Brigham Tavern Lane, Duxbury MA"    TO ADDR.
           MOVE 19                                      TO HOURS.
           MOVE 23.50                                   TO RATE.
           MOVE "PRESIDENT"                             TO JOB.
      *
      ****** Conditional expressions
       CONDITIONAL-SELECTION.
           IF  RATE > 18
               MOVE .25     TO  BONUS-RATE
           ELSE
               MOVE ZERO    TO BONUS-RATE.
           IF JOB = "PRESIDENT"
                MOVE .33    TO BONUS-RATE.
      *
      ****** COBOL DISPLAY statements - Literals assigned to variables
       DISPLAY-DATA-PARAGRAPH.
           COMPUTE GROSS-PAY = (HOURS * RATE) * (1 + BONUS-RATE).
           MOVE RATE            TO RATE-OUT.
           MOVE BONUS-RATE      TO BONUS-RATE-OUT.
           MOVE HOURS           TO HOURS-OUT.
           MOVE GROSS-PAY       TO GROSS-PAY-OUT.
           DISPLAY "Name            : " NAME.
           DISPLAY "Job             : " JOB.
           DISPLAY "Address         : " ADDR.
           DISPLAY "Hours Worked    : " HOURS-OUT.
           DISPLAY "Hourly Rate     : " RATE-OUT.
           DISPLAY "Bonus-Rate      : " BONUS-RATE-OUT.
           DISPLAY "Gross Pay       : " GROSS-PAY-OUT.
           DISPLAY "  ".
           DISPLAY "  ".
       ASSIGN-VP.
           MOVE VP                  TO NAME.
           MOVE "VICE PRESIDENT"    TO JOB.
           MOVE "61 Brigham Tavern Lane, Duxbury MA"    TO ADDR.
           MOVE 30                                      TO HOURS.
           MOVE 20.00                                   TO RATE.


