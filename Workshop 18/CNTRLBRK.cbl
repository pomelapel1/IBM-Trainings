      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *--------------------------------------------------------------
      *  Workshop 18.2 - One level Control Break.
      *  Enhancements by :  Pomela Dominguez
      *
      *  Enhancement Specs:
      *
      *  1. Sort Acct.Data (list of US Presidents terms of
      *     office, address and birth state) by ascending birth state.
      *
      *  2. Generate a report of US Presidents' details per birth
      *     state.
      *
      *  3. Add an additional display column for the Presidents'
      *     accumulated salary during his term of office.
      *
      *  4. Add the following accumulators at the end of the report:
      *     a)  Total of all presidents' salaries (all presidents from
      *         all states.
      *     b)  President that has the highest salary.
      *     c)  President that has the lowest salary.
      *     d)  The average salary for all presidents.
      *
      *  Noticed that there are a number of presidents falling into
      *  the lowest salary category.  To minimize I/O , create an
      *  internal table for easy manipulation of
      *  'End of Report Statistics'.
      *
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSORT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PRINT-REC.
       01  PRINT-REC.
           05 FILLER                    PIC X(03)      VALUE SPACE.
           05 USA-STATE-O               PIC X(18).
           05 FIRST-NAME-O              PIC X(15).
           05 LAST-NAME-O               PIC X(20).
           05 ELECTED-O                 PIC X(6).
           05 LAST-YEAR-O               PIC X(6).
           05 ACCT-LIMIT-O              PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(03)  VALUE SPACES.
           05 ACCT-BALANCE-O            PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(03)  VALUE SPACES.
           05 ACCUM-SALARY-O            PIC $$$,$$$,$$9.99.
           05 FILLER                    PIC X(3)   VALUE SPACES.
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-NO-X REDEFINES ACCT-NO.
               10  START-YEAR     PIC 9(4).
               10  END-YEAR       PIC 9(4).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).  *> Input Sort Key
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.

      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

       01 WS-HEADER-1.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(12)      VALUE
                                                     'Report: A124'.
           05 DATE-O                    PIC X(10)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE SPACES.
           05 FILLER                    PIC X(47)
                                                       VALUE
                              'Presidents Broken Out By State of Birth'.
           05 RPT-DATE                  PIC XXXX/XX/XX.
           05 FILLER                    PIC X(10)      VALUE SPACES.
           05 FILLER                    PIC X(5)       VALUE 'PAGE '.
           05 RPT-PAGE-NO               PIC ZZ.
           05 FILLER                    PIC X(12)      VALUE SPACES.

       01 WS-HEADER-2.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(18)      VALUE 'STATE'.
           05 FILLER                    PIC X(9)       VALUE 'PRESIDENT'
                                                                      .
           05 FILLER                    PIC X(24)      VALUE SPACES.
           05 FILLER                    PIC X(7)       VALUE 'ELECTED'.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(8)       VALUE 'THRU'.
           05 FILLER                    PIC X(14)      VALUE 'SALARY'.
           05 FILLER                    PIC X(18)      VALUE
                                        '   NET WORTH   '.
           05 FILLER                    PIC X(12)      VALUE
                                        'SALARY-ACCUM'.

       01  WS-HEADER-3.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(17)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(32)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(7)       VALUE '======='.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(7)        VALUE '====='.
           05 FILLER                    PIC X(01)      VALUE SPACES.
           05 FILLER                    PIC X(12)       VALUE ALL '='.
           05 FILLER                    PIC X(2)       VALUE SPACES.
           05 FILLER                    PIC X(18)      VALUE
                                                        '============='.
           05 FILLER                    PIC X(12)      VALUE
                                                       '============'.
      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
       01  WS-TRLR-LINE-1.
           05 FILLER                    PIC X(03)       VALUE SPACES.
           05 FILLER                    PIC X(12) VALUE 'Sub Totals:'.
           05 STATE-TRLR-LINE           PIC X(15).
           05 FILLER                    PIC X(37)
              VALUE 'Salary | Net Worth | Accumm Salary: ' JUST RIGHT.
           05 SALARY-SUB-TOT-OUT        PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(02)       VALUE SPACES.
           05 NET-WORTH-SUB-TOT-OUT     PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(3)      VALUE SPACE.
           05 ACC-SALARY-SUB-TOT-OUT    PIC $$$,$$$,$$$.99.
      *-------------------------------------------------------------
      *    End of Report Statistics
      *-------------------------------------------------------------
       01  WS-TRLR-LINE-2.
           05  FILLER                   PIC X(45) VALUE
               "Total of all U.S. Presidents' salaries  : ".
           05  WS-SALARY-TOTAL-OUT      PIC $$$,$$$,$$9.99.
       01  WS-TRLR-LINE-3.
           05  FILLER                   PIC X(45) VALUE
               "The average salary for all presidents   : ".
           05  WS-AVG-SALARY-OUT        PIC $$$,$$$,$$9.99.
       01  WS-TRLR-LINE-4.
           05  FILLER                   PIC X(45) VALUE
               "Presidents with the highest salary      : ".
           05  WS-HIGHSAL-PRESIDENT     PIC X(35).
      *-------------------------------------------------------*
      *    Since there are so many Presidents having the same *
      *    lowest salary, structure of the last stat trailer  *
      *    is done in such a way, that the message            *
      *    'President with the lowest salary' is not repeated.*
      *-------------------------------------------------------*
       01  WS-TRLR-LINE-5.
           05  WS-STAT-MESSAGE          PIC X(45) VALUE
               "President with the lowest salary        : ".
           05  WS-LOWSAL-PRESIDENT      PIC X(35).
      *
       01 WS-COUNTERS-AND-ACCUMULATORS.
           05 WS-LASTREC                PIC X    VALUE SPACE.
           05 WS-LINE-KTR               PIC 9(4) COMP  VALUE 0.
           05 WS-YRS-SERVICE            PIC 99       VALUE 0.
           05 WS-SALARY-SUB-TOT         PIC 9(09)V99 VALUE 0.
           05 WS-NET-WORTH-SUB-TOT      PIC 9(09)V99 VALUE 0.
           05 WS-ACC-SALARY-SUB-TOT     PIC 9(09)V99 VALUE 0.
           05 WS-ACCUM-YRS-SALARY       PIC 9(09)V99 VALUE 0.
           05 WS-SALARY-TOTAL           PIC 9(09)V99 VALUE 0.
           05 WS-HIGHEST-SALARY         PIC 9(08)V99 VALUE 0.
           05 WS-LOWEST-SALARY          PIC 9(08)V99 VALUE 0.
           05 WS-AVG-SALARY             PIC 9(08)V99 VALUE 0.
       01 WS-CONTROL-BREAK-CTR-ACCS.
           05 WS-CONTROL-BREAK-TOTAL    PIC S9(7)V99 COMP-3.
           05 WS-STATE-CTR              PIC  9(2) COMP.
       01 PROGRAM-INDICATOR-SWITCHES.
           05 WS-EOF-INPUT-SW           PIC X(1)       VALUE 'N'.
               88 EOF-INPUT                            VALUE 'Y'.
       01 WS-BREAK-CONTROLS.
           05 WS-CONTROL-KEY            PIC X(15). *> Hold/Control Key
      *--------------------------------------------------------------
      *   Table for End of Report Statistics
      *--------------------------------------------------------------
       01  PRES-ACCOUNTS-TABLE.
           05  PRES-ACCOUNTS OCCURS 45 TIMES
                    ASCENDING KEY IS T-SALARY
                    INDEXED BY T-ACCT-IDX.
               10  T-LAST-NAME         PIC X(20).
               10  T-FIRST-NAME        PIC X(15).
               10  T-SALARY            PIC 9(7)V99.
      *
      *------------------
       PROCEDURE DIVISION.
      *------------------
       000-MAIN-RTN.
      *-----------------------------------------------------*
      *  100-INIT-RTN  Housekeeping, Initial Report Headings
      *-----------------------------------------------------*
           PERFORM 100-INIT-RTN.
           SET T-ACCT-IDX TO 1.
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT.
      *-----------------------------------------------------*
      *  500-CONTROL-BREAK   Final Control Break paragraphs
      *-----------------------------------------------------*
           PERFORM 500-CONTROL-BREAK.
           PERFORM 800-DISPLAY-STATS.
           PERFORM 900-WRAP-UP.
           GOBACK.
      *-----------------------------------------------------*
      *    Housekeeping, Initial Report Headings
      *-----------------------------------------------------*
       100-INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 200-OPEN-FILES.
           MOVE SPACES TO PRINT-REC.
           PERFORM 700-READ-RECORD.
      *-----------------------------------------------------*
      *    Initial Control creates Rpt Headings
      *-----------------------------------------------------*
           PERFORM 500-CONTROL-BREAK.
      *
       150-INIT-WS-FIELDS.
           INITIALIZE WS-CONTROL-BREAK-CTR-ACCS.
      *
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
               IF WS-CONTROL-KEY = USA-STATE *> Control Break Conditional
                   PERFORM 400-MOVE-DATA
                   PERFORM 600-WRITE-DATA
                   PERFORM 700-READ-RECORD
               ELSE
                   PERFORM 500-CONTROL-BREAK
               END-IF
           END-IF.
      *
       400-MOVE-DATA.
           MOVE SPACES TO PRINT-REC.
           ADD +1 TO WS-STATE-CTR.
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State column
                MOVE SPACES TO USA-STATE-O
           ELSE
                MOVE USA-STATE TO USA-STATE-O,  *> MOVE IN-STATE -> HOLD-KEY
                                  STATE-TRLR-LINE
           END-IF.
           ADD ACCT-LIMIT   TO WS-SALARY-SUB-TOT  WS-SALARY-TOTAL.
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT.
      *
      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in office
      *
           MOVE ACCT-NO(1:4) TO ELECTED-O.
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O.
      *
      *    ACCT-NO has been redefined as END-YEAR and START-YEAR.
      *
           COMPUTE WS-YRS-SERVICE =  END-YEAR - START-YEAR.
           COMPUTE WS-ACCUM-YRS-SALARY = (WS-YRS-SERVICE * ACCT-LIMIT).
      *
           ADD  WS-ACCUM-YRS-SALARY  TO WS-ACC-SALARY-SUB-TOT.
      *
           MOVE WS-ACCUM-YRS-SALARY  TO  ACCUM-SALARY-O.
           MOVE ACCT-LIMIT           TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE         TO  ACCT-BALANCE-O.
           MOVE LAST-NAME            TO  LAST-NAME-O.
           MOVE FIRST-NAME           TO  FIRST-NAME-O.
      *
      *--------------------------------------------------------------
      *    Populate the PRES-ACCOUNTS-TABLE  as you process each
      *        record from the ACCTSORT QSAM File.
      *--------------------------------------------------------------
           MOVE LAST-NAME            TO T-LAST-NAME(T-ACCT-IDX).
           MOVE FIRST-NAME           TO T-FIRST-NAME(T-ACCT-IDX).
           MOVE ACCT-LIMIT           TO T-SALARY(T-ACCT-IDX).
           SET T-ACCT-IDX UP BY 1.
       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of program)
                MOVE WS-SALARY-SUB-TOT     TO SALARY-SUB-TOT-OUT
                MOVE WS-NET-WORTH-SUB-TOT  TO NET-WORTH-SUB-TOT-OUT
                MOVE WS-ACC-SALARY-SUB-TOT TO ACC-SALARY-SUB-TOT-OUT
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-TRLR-LINE-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
                ADD 1 TO WS-LINE-KTR
                MOVE ZERO TO WS-SALARY-SUB-TOT WS-NET-WORTH-SUB-TOT
                             WS-ACC-SALARY-SUB-TOT
                MOVE WS-LINE-KTR TO RPT-PAGE-NO
                MOVE USA-STATE TO WS-CONTROL-KEY *> SET NEW CONTROL KEY
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-2
                WRITE PRINT-REC FROM WS-HEADER-3
                PERFORM 150-INIT-WS-FIELDS
           END-IF.

       600-WRITE-DATA.
           WRITE PRINT-REC.

       700-READ-RECORD.
           READ ACCT-REC  AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ.
      *
       800-DISPLAY-STATS.
      *--------------------------------------------------------*
      *    Total of all Presidents' salaries.                  *
      *--------------------------------------------------------*
           COMPUTE WS-SALARY-TOTAL = FUNCTION SUM(T-SALARY(ALL)).
           MOVE    WS-SALARY-TOTAL TO WS-SALARY-TOTAL-OUT.
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           WRITE PRINT-REC FROM WS-TRLR-LINE-2.
      *-------------------------------------------------------*
      *  Compute & print the average salary of the Presidents *
      *-------------------------------------------------------*
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           SET T-ACCT-IDX TO 1.
           COMPUTE WS-AVG-SALARY = FUNCTION MEAN(T-SALARY(ALL)).
           MOVE WS-AVG-SALARY TO WS-AVG-SALARY-OUT.
           WRITE PRINT-REC FROM WS-TRLR-LINE-3.
      *--------------------------------------------------------*
      *  Identify the highest-paid President                 *
      *--------------------------------------------------------*
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           SET T-ACCT-IDX TO 1.
           COMPUTE WS-HIGHEST-SALARY = FUNCTION MAX(T-SALARY(ALL)).
           SEARCH  PRES-ACCOUNTS VARYING T-ACCT-IDX
               AT END  MOVE 'Highest paid President not found!!! ' TO
                       WS-HIGHSAL-PRESIDENT
               WHEN WS-HIGHEST-SALARY = T-SALARY(T-ACCT-IDX)
                  STRING T-LAST-NAME(T-ACCT-IDX) DELIMITED BY "  "
                          ",  "     DELIMITED BY SIZE
                         T-FIRST-NAME(T-ACCT-IDX)  DELIMITED BY "  "
                         INTO WS-HIGHSAL-PRESIDENT
                END-STRING
           END-SEARCH.
           WRITE PRINT-REC FROM WS-TRLR-LINE-4.
      *
      *--------------------------------------------------------*
      *      Identify the lowest-paid President                *
      *
      *--------------------------------------------------------*
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           SET T-ACCT-IDX TO 1.
           COMPUTE WS-LOWEST-SALARY = FUNCTION MIN(T-SALARY(ALL)).
           PERFORM VARYING T-ACCT-IDX FROM 1 BY 1 UNTIL
                   T-ACCT-IDX > 45
               IF  WS-LOWEST-SALARY = T-SALARY(T-ACCT-IDX)
                   STRING T-LAST-NAME(T-ACCT-IDX) DELIMITED BY "  "
                         ",  "     DELIMITED BY SIZE
                          T-FIRST-NAME(T-ACCT-IDX) DELIMITED BY "  "
                          INTO WS-LOWSAL-PRESIDENT
                   END-STRING
                   WRITE PRINT-REC FROM WS-TRLR-LINE-5
                   MOVE SPACES TO WS-STAT-MESSAGE
              END-IF
           END-PERFORM.

       900-WRAP-UP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.