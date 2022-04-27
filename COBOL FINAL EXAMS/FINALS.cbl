       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALS.
       AUTHOR. POMELA DOMINGUEZ.
      *TEACHER.  THE GREAT JONATHAN SAYLES.
      *---------------------------------------------------------------
      *   REVISED :  OCTOBER 1, 2020 12.52 EST.
      **** Final Project for Cobol for Business Application ******
      *
      *  PREREQUISITE:  Input file, PARTSUPP, has to be pre-sorted by
      *  Part-Number.
      *
      *  Program Specifications:
      *  FINALS program reads a pre-sorted PARTSUPP input file, a
      *  file of Transaction Records for PARTS, with its SUPPLIERS and
      *  the PURCHASE ORDERS on these PARTS. It also has a record of
      *  the SUPPLIERS addresses.   FINALS program reads the PARTSUPP
      *  file and validates each segment of the PARTSUPP.
      *
      *  Subroutines:
      *  PROCESS-PARTEDIT PARAGRAPH.
      *  - prepares the parameters to be passed to PARTEDIT subroutine.
      *  - whatever is passed back by PARTEDIT, PART record validated
      *    is written to either Exception Report or Parts Transaction
      *    Report.
      *  - parameters passed are as follows:
      *  CALL 'PARTEDIT' USING DATA-SEND-TOPARTEDIT, TOTAL-ERROR,
      *                  EXCP-DESC.
      *  TOTAL-ERROR  - accounts the number of validation errors for
      *        the whole record, which includes the PARTS, SUPPLIER
      *        ADDRESS & PO portion.
      *  VALID-PART - a return flag to indicate whether the record
      *         validated is valid or not.
      *  EXCP-DESC - returned by the subroutine to indicate why the
      *        the field/(s) are invalid.
      *
      *  PROCESS-SUPPEDIT-PARAGRAPH.
      *  - follows the same concept as the PARTEDIT.
      *
      *  PROCESS-ADDREDIT.
      *  - the same concept is followed as with the rest of the EDIT
      *    paragraphs, except that an array is passed to ADDREDIT.
      *    Prior to the call of ADDREDIT,  the array has been setup
      *    from the main program.
      *    The same array will be read after the ADDREDIT call to
      *    print the three (3) addresses if the record is valid.
      *
      *  PROCESS-POEDIT.
      * - the same concept as with PARTEDIT, an array is passed to the
      *   subroutine.
      *   CEEDAYS will be used to validate the dates in this subroutine.
      *
      *
      *********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PART-TRANS-FILE    ASSIGN TO PARTSIN.
           SELECT STATES-FILE        ASSIGN TO STATEZIP.
           SELECT PARTS-FILE         ASSIGN TO PARTS.
           SELECT SUPPLIER-FILE      ASSIGN TO SUPPLIER.
           SELECT ADDRESS-FILE       ASSIGN TO ADDRESES.
           SELECT PURCHASES-FILE     ASSIGN TO PURCHASE.
           SELECT PARTS-REPORT       ASSIGN TO PRTFILE.
           SELECT EXCEPTION-REPORT   ASSIGN TO EXCEPTNS.
       DATA DIVISION.
       FILE SECTION.
       FD  PART-TRANS-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PART-SUPP-ADDR-PO.
       COPY PARTSUPP.
      *
       FD  STATES-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS STATES-REC.
       01  STATES-REC.
           05 STATE-NAME              PIC X(16).
           05 STATE-ID                PIC X(2).
           05 FILLER                  PIC X(2) VALUE SPACES.
           05 STATE-LOW-ZIP           PIC 9(5).
           05 FILLER                  PIC X(3) VALUE SPACES.
           05 STATE-HIGH-ZIP          PIC 9(5).
           05 FILLER                  PIC X(47) VALUE SPACES.
      *
       FD  PARTS-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 78 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTS-REC.
       COPY PARTS.
      *
       FD  SUPPLIER-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 38 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SUPPLIER-REC.
       COPY SUPPLIER.
      *
       FD  ADDRESS-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 78 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SUPP-ADDRESS.
        COPY ADDRESES.
      *
       FD  PURCHASES-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 35 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PURCHASE-ORDERS.
       COPY PURCHASE.
      *
       FD  EXCEPTION-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS EXCP-REPORT-REC.
       01  EXCP-REPORT-REC.
           05  FILLER                         PIC X    VALUE SPACE.
           05  EXCP-TRANS-RECORD              PIC X(80).
           05  EXCP-REASON                    PIC X(50).
      *
       FD  PARTS-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTS-REPORT-REC.
       01  PARTS-REPORT-REC.
           05  FILLER               PIC X    VALUE SPACE.
           05  PART-NAME-OUT        PIC X(14).
           05  FILLER               PIC X(11)   VALUE SPACES.
           05  WEEKS-LEAD-TIME-OUT  PIC ZZ9.
           05  FILLER               PIC X(11)   VALUE SPACES.
           05  VEHICLE-MAKE-OUT     PIC X(20).
           05  FILLER               PIC X(5)    VALUE SPACES.
           05  SUPPLIER-NAME-OUT    PIC X(15).
           05  FILLER               PIC X(5)    VALUE SPACES.
           05  SUPPLIER-RATING-OUT  PIC X(20).
           05  FILLER               PIC X(2)    VALUE SPACES.
      *
       WORKING-STORAGE SECTION.
       01  DATE-VARS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR  PIC 9(4).
               10  WS-CURRENT-MONTH PIC 9(2).
               10  WS-CURRENT-DAY   PIC 9(2).
      *
       01  HEADER-LINE-0.
           05  FILLER               PIC X      VALUE SPACE.
           05  FILLER               PIC X(55)  VALUE SPACES.
           05  FILLER               PIC X(16)  VALUE
               'MYSTERIOUS INC. '.
      *
       01  HEADER-LINE-1.
           05  FILLER               PIC X      VALUE SPACE.
           05  FILLER               PIC X(6)   VALUE 'DATE: '.
           05  WS-CURR-YEAROUT      PIC 9(4).
           05  FILLER               PIC X      VALUE '/'.
           05  WS-CURR-MONTHOUT     PIC 9(2).
           05  FILLER               PIC X      VALUE '/'.
           05  WS-CURR-DAYOUT       PIC 9(2).
           05  FILLER               PIC X(20)  VALUE SPACES.
           05  FILLER               PIC X(40)  VALUE
                     '  Supplier Parts Transaction Records  '.
           05  FILLER               PIC X(45)  VALUE SPACES.
       01  HEADER-LINE-2.
           05  FILLER               PIC X      VALUE SPACE.
           05  FILLER               PIC X(14)  VALUE 'Part Name  '.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE
                                       'Weeks Lead Time'.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(20)  VALUE
                                       'Vehicle Make '.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE 'Supplier Name'.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE
                                       'Supplier Rating'.
           05  FILLER               PIC X(2)   VALUE SPACES.
      *
       01  HEADER-LINE-3.
           05  FILLER               PIC X      VALUE SPACE.
           05  FILLER               PIC X(14)  VALUE '=============='.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE
                                       '==============='.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(20)  VALUE
                                      '===================='.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE '==============='.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  FILLER               PIC X(15)  VALUE
                                       '==============='.
           05  FILLER               PIC X(2)   VALUE SPACES.
      *
       01  FOOTER-LINE-0.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(20) VALUE
              'Order Address  : '.
           05 ORDER-ADDR-OUT            PIC X(70).
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-LINE-1.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(20) VALUE
              'Sched Address  : '.
           05 SCHED-ADDR-OUT            PIC X(70).
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-LINE-2.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(20) VALUE
              'Remit Address  : '.
           05 REMIT-ADDR-OUT            PIC X(70).
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-LINE-3.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(35) VALUE
              'Total # Purchase Orders : '.
           05 TOT-NUM-POS-OUT           PIC 99.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-4.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(35) VALUE
              'Total Amount in Purchase Orders : '.
           05 TOT-PRICE-POS-OUT         PIC 9999.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-5.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(35) VALUE
              'Total Qty in Purchase Orders : '.
           05 TOT-QTY-POS-OUT           PIC 9999.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-STAT-0.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total Part Transaction Records Read    : '.
           05 TOT-PART-TRANS-READ       PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-1.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total Valid Part Transactions Reported : '.
           05 TOT-PART-TRANS-ONREPORT   PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-2.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total PART Records Written             : '.
           05 TOT-PART-WRITTEN          PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-3.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total Part Records on Exception        : '.
           05 TOT-PART-EXCEPTION        PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-4.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total SUPPLIER Records Written         : '.
           05 TOT-SUPPLIER-WRITTEN      PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-5.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total SUPPLIER Records on Exception    : '.
           05 TOT-SUPPLIER-EXCEPTION    PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-6.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total ADDRESS Records Written          : '.
           05 TOT-ADDRESS-WRITTEN       PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-7.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total ADDRESS Records on Exception     : '.
           05 TOT-ADDRESS-EXCEPTION     PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-8.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total PO Records Written               : '.
           05 TOT-PO-WRITTEN            PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
       01  FOOTER-STAT-9.
           05 FILLER                    PIC X     VALUE SPACE.
           05 FILLER                    PIC X(50) VALUE
              'Total PO Records on Exception          : '.
           05 TOT-PO-EXCEPTION          PIC 99.
           05 FILLER                    PIC X(30) VALUE SPACES.
      *
      *
       01  TMPS-ACC-CTRS-SWITCHES.
           05  TOT-PRICE               PIC S9(9)V99 VALUE ZEROES.
           05  TOTAL-ERROR             PIC 99       VALUE ZEROES.
           05  ERROR-CTR               PIC 9        VALUE ZEROES.
           05  TEMP-ADDRESS            PIC X(70)    VALUE SPACES.
           05  ADD-TO-ADDRESS          PIC X(15)    VALUE SPACES.
           05  VALID-RECORD            PIC 9        VALUE ZERO.
           05  EXCP-DESC               PIC X(50)    VALUE SPACES.
           05  EOF-STAT             PIC X        VALUE SPACE.
               88  END-OF-FILE      VALUE 'Y'.
      *

       01  DATA-TOSEND-ADDREDIT.
           05 STATE-ZIP-TABLE OCCURS 72 TIMES
                   ASCENDING KEY IS WS-STATE-ID INDEXED BY ZIP-IDX.
              10 WS-STATE-NAME              PIC X(16).
              10 WS-STATE-ID                PIC X(2).
              10 WS-STATE-LOW-ZIP           PIC 9(5).
              10 WS-STATE-HIGH-ZIP          PIC 9(5).
      *
       01  WS-USER-ABEND-CODE               PIC S9(04) COMP VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INITIALIZE-STAT-TOTALS.
           PERFORM OPEN-FILES.
           PERFORM LOAD-STATES-TO-TABLE.
           MOVE   'N' TO EOF-STAT.
           READ    PART-TRANS-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
                   ADD 1 TO TOT-PART-TRANS-READ.
           IF  END-OF-FILE
               PERFORM ABEND-AND-RETURN
           END-IF.
           INITIALIZE EXCP-REPORT-REC PARTS-REPORT-REC.
           PERFORM PART-REPORT-HEADINGS.
           PERFORM EXCEPTION-HEADINGS.
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
           PERFORM WRITE-STATISTICS.
           PERFORM CLOSE-FILES.
           GOBACK.
      *
       PROCESS-RECORDS.
      *
           MOVE 0      TO TOTAL-ERROR.

           MOVE SPACES TO EXCP-DESC.
           PERFORM  PROCESS-PARTEDIT.

           IF  TOTAL-ERROR < 4
               MOVE SPACES TO EXCP-DESC
               PERFORM  PROCESS-SUPPEDIT
           END-IF.

           IF  TOTAL-ERROR < 4
               MOVE SPACES TO EXCP-DESC
               PERFORM  PROCESS-ADDREDIT
           END-IF.

           IF  TOTAL-ERROR < 4
               MOVE SPACES TO EXCP-DESC
               PERFORM  PROCESS-POEDIT
           END-IF.

           IF  TOTAL-ERROR < 4
               PERFORM WRITE-PARTS-REPORT-DATA
               PERFORM REPORT-ADDRESS-FOOTERS
               PERFORM REPORT-PO-FOOTERS
               ADD 1 TO TOT-PART-TRANS-ONREPORT
           END-IF.

           MOVE SPACES TO  PARTS-REPORT-REC  EXCP-REPORT-REC.
           READ  PART-TRANS-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
           ADD 1 TO TOT-PART-TRANS-READ.
      *
       PROCESS-PARTEDIT.
           INITIALIZE  PARTS-REC, ERROR-CTR,
                       VALID-RECORD, EXCP-DESC.
           MOVE CORRESPONDING PARTS TO PARTS-REC.

      *-------------------------------------------------------------
      *    DATA SEND TO PARTEDIT WILL BE THE PARTS-REC AND THE
      *    VALID-RECORD INDICATOR ALONG WITH THE ERROR-CTR &
      *    EXCP-DESC.
      *-------------------------------------------------------------
           CALL 'PARTEDIT' USING PARTS-REC, VALID-RECORD,
                                 ERROR-CTR, EXCP-DESC.

           ADD ERROR-CTR TO TOTAL-ERROR.

           IF  VALID-RECORD = 0
               WRITE PARTS-REC
               ADD 1 TO TOT-PART-WRITTEN
           ELSE
               MOVE  PARTS-REC TO EXCP-TRANS-RECORD
               MOVE  EXCP-DESC TO EXCP-REASON
               WRITE EXCP-REPORT-REC
               ADD 1 TO TOT-PART-EXCEPTION
           END-IF.
      *
       PROCESS-SUPPEDIT.
           INITIALIZE  SUPPLIER-REC, ERROR-CTR,
                           VALID-RECORD, EXCP-DESC.

           MOVE CORRESPONDING SUPPLIERS TO SUPPLIER-REC.

      *-------------------------------------------------------------
      *    DATA SEND TO SUPPEDIT WILL BE THE SUPPLIER-REC AND THE
      *    VALID-SUPPLIER INDICATOR ALONG WITH THE TOTAL-ERROR &
      *    EXCP-DESC.
      *-------------------------------------------------------------
           CALL 'SUPPEDIT' USING SUPPLIER-REC, VALID-RECORD,
                                 ERROR-CTR, EXCP-DESC.

           ADD ERROR-CTR TO TOTAL-ERROR.

           IF VALID-RECORD = 0
              WRITE SUPPLIER-REC
              ADD 1 TO TOT-SUPPLIER-WRITTEN
           ELSE
              MOVE  SUPPLIERS TO EXCP-TRANS-RECORD
              MOVE  EXCP-DESC TO EXCP-REASON
              WRITE EXCP-REPORT-REC
              ADD 1 TO TOT-SUPPLIER-EXCEPTION
           END-IF.
      *
       PROCESS-ADDREDIT.
      *
           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL
                   ADDR-IDX > 3

           INITIALIZE SUPP-ADDRESS, ERROR-CTR,
                           VALID-RECORD, EXCP-DESC

              MOVE CORRESPONDING SUPPLIER-ADDRESS(ADDR-IDX)
                   TO SUPP-ADDRESS

             CALL 'ADDREDIT' USING  SUPP-ADDRESS, VALID-RECORD,
                    ERROR-CTR, EXCP-DESC, DATA-TOSEND-ADDREDIT

              ADD ERROR-CTR TO TOTAL-ERROR

              IF  VALID-RECORD = 0
                  WRITE SUPP-ADDRESS
                  ADD 1 TO TOT-ADDRESS-WRITTEN
              ELSE
                  MOVE  SUPPLIER-ADDRESS(ADDR-IDX) TO
                        EXCP-TRANS-RECORD
                  MOVE  EXCP-DESC  TO EXCP-REASON
                  WRITE EXCP-REPORT-REC
                  ADD 1 TO TOT-ADDRESS-EXCEPTION
              END-IF

           END-PERFORM.

      *
       PROCESS-POEDIT.
      *
           PERFORM VARYING PO-IDX FROM 1 BY 1 UNTIL
                   PO-IDX > 3

           INITIALIZE PURCHASE-ORDERS, ERROR-CTR,
                           VALID-RECORD

              MOVE CORRESPONDING PURCHASE-ORDER(PO-IDX)
                   TO PURCHASE-ORDERS

              CALL 'POEDIT' USING  PURCHASE-ORDERS, VALID-RECORD,
                    ERROR-CTR, EXCP-DESC

              ADD ERROR-CTR TO TOTAL-ERROR

              IF  VALID-RECORD = 0
                  WRITE PURCHASE-ORDERS
                  ADD 1 TO TOT-PO-WRITTEN
              ELSE
                  MOVE  PURCHASE-ORDER(PO-IDX) TO
                        EXCP-TRANS-RECORD
                  MOVE  EXCP-DESC  TO EXCP-REASON
                  WRITE EXCP-REPORT-REC
                  ADD 1 TO TOT-PO-EXCEPTION
              END-IF
           END-PERFORM.

      *
       WRITE-PARTS-REPORT-DATA.
      *
           MOVE PART-NAME       IN PARTS   TO PART-NAME-OUT.
           MOVE WEEKS-LEAD-TIME IN PARTS   TO WEEKS-LEAD-TIME-OUT.
           EVALUATE TRUE
              WHEN CHRYSLER   IN PARTS
                              MOVE 'Chrysler ' TO  VEHICLE-MAKE-OUT
              WHEN FORD       IN PARTS
                              MOVE 'Ford '     TO  VEHICLE-MAKE-OUT
              WHEN GM         IN PARTS
                            MOVE 'General Motors' TO  VEHICLE-MAKE-OUT
              WHEN VOLKSWAGON IN PARTS
                              MOVE 'Volkswagen' TO  VEHICLE-MAKE-OUT
              WHEN TOYOTA     IN PARTS
                              MOVE 'Toyota   ' TO  VEHICLE-MAKE-OUT
              WHEN JAGUAR     IN PARTS
                              MOVE 'Jaguar   ' TO  VEHICLE-MAKE-OUT
              WHEN PEUGEOT    IN PARTS
                              MOVE 'Peugeot  ' TO  VEHICLE-MAKE-OUT
              WHEN BMW        IN PARTS
                              MOVE 'BMW      ' TO  VEHICLE-MAKE-OUT
           END-EVALUATE.
           MOVE SUPPLIER-NAME   IN SUPPLIERS TO SUPPLIER-NAME-OUT.

           EVALUATE  TRUE
              WHEN HIGHEST-QUALITY IN SUPPLIERS
                   MOVE 'HIGHEST QUALITY' TO SUPPLIER-RATING-OUT
              WHEN AVERAGE-QUALITY IN SUPPLIERS
                   MOVE 'AVERAGE QUALITY' TO SUPPLIER-RATING-OUT
              WHEN LOWEST-QUALITY  IN SUPPLIERS
                   MOVE 'LOWEST QUALITY' TO SUPPLIER-RATING-OUT
           END-EVALUATE.
           WRITE   PARTS-REPORT-REC.
      *
       REPORT-ADDRESS-FOOTERS.
      * -----------------------------------------------------------*
      *  NEW SUBROUTINE WITHOUT THE USE OF TABLE
      *------------------------------------------------------------*

           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX > 3
               MOVE SPACES TO TEMP-ADDRESS ADD-TO-ADDRESS
               IF   ADDRESS-3 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 NOT EQUAL SPACES
                    MOVE ADDRESS-3 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 TO ADD-TO-ADDRESS
                    PERFORM STRING-THE-ADDRESS
               END-IF

               IF  ADDRESS-2 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 NOT EQUAL SPACES
                   MOVE SPACES TO ADD-TO-ADDRESS
                   MOVE ADDRESS-2 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 TO ADD-TO-ADDRESS
                   PERFORM STRING-THE-ADDRESS
               END-IF
               IF  ADDRESS-1 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 NOT EQUAL SPACES
                   MOVE SPACES TO ADD-TO-ADDRESS
                   MOVE ADDRESS-1 IN SUPPLIER-ADDRESS(ADDR-IDX)
                                 TO ADD-TO-ADDRESS
                   PERFORM STRING-THE-ADDRESS
               END-IF
               STRING  TEMP-ADDRESS DELIMITED BY '  '
                       ', '           DELIMITED BY SIZE
                       CITY IN SUPPLIER-ADDRESS(ADDR-IDX)
                                     DELIMITED BY '  '
                       ', '          DELIMITED BY SIZE
                       ADDR-STATE IN SUPPLIER-ADDRESS(ADDR-IDX)
                                     DELIMITED BY '  '
                       ', '          DELIMITED BY SIZE
                       ZIP-CODE IN SUPPLIER-ADDRESS(ADDR-IDX) (1:5)
                                     DELIMITED BY SIZE
                   INTO TEMP-ADDRESS
              EVALUATE TRUE
                  WHEN ADDRESS-TYPE IN SUPPLIER-ADDRESS(ADDR-IDX) = '1'
                       MOVE TEMP-ADDRESS TO  ORDER-ADDR-OUT
                  WHEN ADDRESS-TYPE IN SUPPLIER-ADDRESS(ADDR-IDX) = '2'
                       MOVE TEMP-ADDRESS TO  SCHED-ADDR-OUT
                  WHEN ADDRESS-TYPE IN SUPPLIER-ADDRESS(ADDR-IDX) = '3'
                       MOVE TEMP-ADDRESS TO  REMIT-ADDR-OUT
              END-EVALUATE
           END-PERFORM.

           MOVE  SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-0.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-1.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-2.

      *
       STRING-THE-ADDRESS.
           IF TEMP-ADDRESS EQUAL SPACES
              STRING ADD-TO-ADDRESS  DELIMITED BY SIZE
                ' ' DELIMITED BY SIZE
              INTO TEMP-ADDRESS
           ELSE
              STRING TEMP-ADDRESS DELIMITED BY '  '
                ' ' DELIMITED BY SIZE
                ADD-TO-ADDRESS DELIMITED BY SIZE
                    INTO TEMP-ADDRESS
           END-IF.
      *
       REPORT-PO-FOOTERS.
           MOVE ZEROES TO TOT-NUM-POS-OUT, TOT-PRICE, TOT-QTY-POS-OUT
                          TOT-PRICE-POS-OUT.
           PERFORM VARYING PO-IDX FROM 1 BY 1 UNTIL PO-IDX > 3
             ADD 1                          TO TOT-NUM-POS-OUT
             ADD UNIT-PRICE IN PURCHASE-ORDER(PO-IDX) TO TOT-PRICE
             ADD UNIT-PRICE IN PURCHASE-ORDER(PO-IDX) TO TOT-QTY-POS-OUT
           END-PERFORM.

           MOVE  SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           MOVE  TOT-PRICE TO TOT-PRICE-POS-OUT.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-3.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-4.
           WRITE PARTS-REPORT-REC FROM FOOTER-LINE-5.

           MOVE  SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.

      *
       PART-REPORT-HEADINGS.
           MOVE SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC   FROM   HEADER-LINE-0.
           MOVE SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           MOVE  FUNCTION  CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE  WS-CURRENT-YEAR        TO WS-CURR-YEAROUT.
           MOVE  WS-CURRENT-MONTH       TO WS-CURR-MONTHOUT.
           MOVE  WS-CURRENT-DAY         TO WS-CURR-DAYOUT.
           WRITE PARTS-REPORT-REC   FROM   HEADER-LINE-1.

           MOVE  SPACES     TO     PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC   FROM   HEADER-LINE-2.
           WRITE PARTS-REPORT-REC   FROM   HEADER-LINE-3.
           MOVE  SPACES     TO     PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
      *
       EXCEPTION-HEADINGS.
           MOVE  'MYSTERIOUS INC.                      '
                  TO EXCP-TRANS-RECORD.
           WRITE EXCP-REPORT-REC.
           MOVE SPACES TO EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
           MOVE  'EXCEPTION REPORT ON TRANSACTION PARTS'
                              TO EXCP-TRANS-RECORD.
           WRITE EXCP-REPORT-REC.
           MOVE SPACES TO EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
           MOVE
                 '  PARTS / SUPPLIER / ADDRESS / PO EXCEPTION RECORDS '
                                        TO EXCP-TRANS-RECORD.
           MOVE  'REASON      ' TO EXCP-REASON.
           WRITE EXCP-REPORT-REC.
           MOVE  '  =================================================='
                                        TO EXCP-TRANS-RECORD.
           MOVE  '====================' TO EXCP-REASON.
           WRITE EXCP-REPORT-REC.
           MOVE  SPACES TO EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
      *
       ABEND-AND-RETURN.
           DISPLAY ' '.
           DISPLAY ' *************************************'.
           DISPLAY ' *** Program: FINALS               ***'.
           DISPLAY ' *** FILE: PART_TRANS_FILE         ***'.
           DISPLAY ' ***                               ***'.
           DISPLAY ' *** No Input Data                 ***'.
           DISPLAY ' ***                               ***'.
           DISPLAY ' *************************************'.
           MOVE +40                TO WS-USER-ABEND-CODE.
           CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE.
      *
       LOAD-STATES-TO-TABLE.
           MOVE 'N' TO EOF-STAT.
           READ  STATES-FILE AT END MOVE 'Y' TO EOF-STAT.
           SET ZIP-IDX TO 1.
           PERFORM VARYING ZIP-IDX FROM 1 BY 1 UNTIL ZIP-IDX > 72
                   OR  END-OF-FILE
              MOVE STATE-NAME     TO WS-STATE-NAME(ZIP-IDX)
              MOVE STATE-ID       TO WS-STATE-ID(ZIP-IDX)
              MOVE STATE-LOW-ZIP  TO WS-STATE-LOW-ZIP(ZIP-IDX)
              MOVE STATE-HIGH-ZIP TO WS-STATE-HIGH-ZIP(ZIP-IDX)

              DISPLAY 'STATE ZIP TABLE ' STATE-ZIP-TABLE(ZIP-IDX)

              READ STATES-FILE
                   AT END MOVE 'Y' TO EOF-STAT
              END-READ
           END-PERFORM.
      *
       WRITE-STATISTICS.
           MOVE  SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-0.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-1.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-2.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-3.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-4.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-5.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-6.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-7.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-8.
           WRITE PARTS-REPORT-REC FROM FOOTER-STAT-9.
      *
           MOVE  SPACES TO PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.
           WRITE PARTS-REPORT-REC.


       INITIALIZE-STAT-TOTALS.
           INITIALIZE  TOT-PART-TRANS-READ, TOT-PART-TRANS-ONREPORT,
                       TOT-PART-WRITTEN, TOT-PART-EXCEPTION,
                       TOT-SUPPLIER-WRITTEN, TOT-SUPPLIER-EXCEPTION,
                       TOT-ADDRESS-WRITTEN,  TOT-ADDRESS-EXCEPTION,
                       TOT-PO-WRITTEN, TOT-PO-EXCEPTION.
      *
       OPEN-FILES.
           OPEN INPUT  PART-TRANS-FILE, STATES-FILE.
           OPEN OUTPUT PARTS-FILE, SUPPLIER-FILE, ADDRESS-FILE,
                       PURCHASES-FILE, PARTS-REPORT, EXCEPTION-REPORT.
      *
       CLOSE-FILES.
           CLOSE  PART-TRANS-FILE, STATES-FILE.
           CLOSE  PARTS-FILE, SUPPLIER-FILE, ADDRESS-FILE,
                       PURCHASES-FILE, PARTS-REPORT, EXCEPTION-REPORT.