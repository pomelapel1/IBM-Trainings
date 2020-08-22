       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERM.
      *
      ********** Workshop 12  -   Pomela Dominguez    ************
      *
      *   Midterm Examination.
      *   a)  To generate the Insurance Claims Report for all claims
      *       that are "In-Policy".
      *
      *   Assumptions:
      *
      *   Since Policy Amount as per Program Specification, is
      *   the amount currently left on the Policy, I set up
      *   a standard Max Coverage Amount for each policy defined.
      *
      *   Max-Coverage-amount is the same for the same 'Policy Type'.
      *   Max-Coverage-amount is as per the declaration below.
      *
      *   77  MAX-COVER-AMT-PRIVATE    PIC S9(7)V99 VALUE 100000.00.
      *   77  MAX-COVER-AMT-MEDICARE   PIC S9(7)V99 VALUE 50000.00.
      *   77  MAX-COVER-AMT-AFFORDABLE PIC S9(7)V99 VALUE 25000.00.
      *
      *   Deductible is 2% for the Maximum Coverage Amt per policy type.
      *   This is as per specification #2.
      *
      *   Insurance benefit period is assumed to cover 1 year
      *       from the date policy is written.
      *
      *
      *
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE      ASSIGN TO INSCLAIM.
           SELECT CLAIMS-REPORT    ASSIGN TO CLAIMRPT.
           SELECT EXCEPTION-REPORT ASSIGN TO EXCEPTNS.
       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMS-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS CLAIM-RECORD-WS.
           COPY CLAIMREC.
      *
       FD  EXCEPTION-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 110 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS EXCP-REPORT-REC.
       01  EXCP-REPORT-REC.
           05  EXCP-CLAIM-RECORD              PIC X(80).
           05  EXCP-REASON                    PIC X(30).
      *
       FD  CLAIMS-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS CLAIM-REPORT-REC.
       01  CLAIM-REPORT-REC.
           05  POLICY-TYPE-DESC           PIC X(20).
           05  FILLER                     PIC X(2)    VALUE SPACES.
           05  INSURED-POLICY-NO-OUT      PIC 9(07).
           05  FILLER                     PIC X(2)    VALUE SPACES.
           05  INSURED-LASTNAME-OUT       PIC X(15).
           05  FILLER                     PIC X(2)    VALUE ", ".
           05  INSURED-FIRSTNAME-OUT      PIC X(10).
           05  FILLER                     PIC X(2)    VALUE SPACES.
           05  POLICY-DATE-OUT            PIC XXXX/XX/XX.
           05  FILLER                     PIC X(2)    VALUE SPACES.
           05  POLICY-RENEW-DATE-OUT      PIC XXXX/XX/XX.
           05  FILLER                     PIC X(6)    VALUE SPACES.
           05  POLICY-DEDUCT-PD-OUT       PIC X.
               88  DEDUCTIBLE-PAID        VALUE 'Y'.
           05  FILLER                     PIC X(7)    VALUE SPACES.
           05  POLICY-COPAY-OUT           PIC .99.
           05  FILLER                     PIC X(5)    VALUE SPACES.
           05  POLICY-AVAIL-AMT-OUT       PIC $$,$$$,$$$.99CR.
           05  FILLER                     PIC X(5)    VALUE SPACES.
           05  POLICY-DED-OUT             PIC $$,$$9CR.
           05  FILLER                     PIC X(2).
           05  CLAIM-AMT-OUT              PIC $$,$$$,$$$.99CR.
           05  FILLER                     PIC X(2).
           05  CLAIM-PAID-OUT             PIC $$,$$$,$$$.99CR.
      *
       WORKING-STORAGE SECTION.
       77  MAX-COVER-AMT-PRIVATE        PIC S9(7)V99 VALUE 100000.00.
       77  MAX-COVER-AMT-MEDICARE       PIC S9(7)V99 VALUE 75000.00.
       77  MAX-COVER-AMT-AFFORDABLE     PIC S9(7)V99 VALUE 50000.00.
      *
       77  MAX-DED-AMT-PRIVATE          PIC S9(4)   VALUE  2000.
       77  MAX-DED-AMT-MEDICARE         PIC S9(4)   VALUE  1500.
       77  MAX-DED-AMT-AFFORDABLE       PIC S9(4)   VALUE  1000.
      *
       01  DATE-VARS.
           05  INT-POLICY-RENEWAL-DATE  PIC 9(7).
      *
           05  WS-RENEWAL-DATE          PIC 9(8).
           05  WS-RENEW-DATE-REDEF REDEFINES WS-RENEWAL-DATE PIC X(8).
      *         10  WS-RENEW-YEAR        PIC 9(4).
      *         10  WS-RENEW-MONTH       PIC 9(2).
      *         10  WS-RENEW-DAY         PIC 9(2).
      *
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR      PIC 9(4).
               10  WS-CURRENT-MONTH     PIC 9(2).
               10  WS-CURRENT-DAY       PIC 9(2).
      *
       01  ACC-CTRS-SWITCHES.
           05  TOT-CLAIM-AMOUNT         PIC S9(8)V99 VALUE ZEROES.
           05  TOT-CLAIM-PAID           PIC S9(8)V99 VALUE ZEROES.
           05  TOT-CLAIMS-PRCSD         PIC 999      VALUE ZEROES.
           05  TOT-CLAIMS-RJCTD         PIC 999      VALUE ZEROES.
      *
           05  EOF-STAT                 PIC X        VALUE SPACE.
               88  END-OF-FILE          VALUE 'Y'.
      *
           05  EXCPTN-STAT              PIC X       VALUE SPACE.
               88  WRITE-EXCEPTION      VALUE 'Y'.
     *
      *
       01  HEADER-LINE-1.
           05  FILLER                   PIC X(6)  VALUE 'DATE: '.
           05  WS-CURR-YEAROUT          PIC 9(4).
           05  FILLER                   PIC X     VALUE '/'.
           05  WS-CURR-MONTHOUT         PIC 9(2).
           05  FILLER                   PIC X     VALUE '/'.
           05  WS-CURR-DAYOUT           PIC 9(2).
           05  FILLER                   PIC X(50) VALUE SPACES.
           05  FILLER                   PIC X(26) VALUE
                     '  Daily Insurance Claims  '.
           05  FILLER                   PIC X(45) VALUE SPACES.
       01  HEADER-LINE-2.
           05  FILLER                   PIC X(4)  VALUE SPACE.
           05  FILLER                   PIC X(11) VALUE 'POLICY TYPE'.
           05  FILLER                   PIC X(7)  VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE 'POLICY #'.
           05  FILLER                   PIC X(10) VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE 'INSURED'.
           05  FILLER                   PIC X(12) VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE 'POLICY EFF'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE ' RENEWAL'.
           05  FILLER                   PIC X(1)  VALUE SPACES.
           05  FILLER                   PIC X(11) VALUE 'DDCTBLE PD?'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(6)  VALUE 'COPAY%'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(15) VALUE
                                                 'CURRENT BALANCE'.
           05  FILLER                   PIC X(2)  VALUE SPACE.
           05  FILLER                   PIC X(13) VALUE 'DEDUCTIBLE PD'.
           05  FILLER                   PIC X(5)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE 'CLAIM AMT'.
           05  FILLER                   PIC X(5) VALUE SPACES.
           05  FILLER                   PIC X(10)  VALUE 'CLAIM PAID'.
      *
       01  HEADER-LINE-3.
      *
           05  FILLER                   PIC X(20)
                     VALUE '===================='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE '======='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(27) VALUE
                                   '==========================='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE '=========='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE '=========='.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE '======='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE '========'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(15)  VALUE
                                                 '==============='.
           05  FILLER                   PIC X(3)  VALUE SPACE.
           05  FILLER                   PIC X(13) VALUE '============='.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE '=========='.
           05  FILLER                   PIC X(5)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE '=========='.
      *
       01  FOOTER-LINE-0.
           05 FILLER                    PIC X(70) VALUE SPACES.
           05 FILLER                    PIC X(50) VALUE
                "************  Nothing follows  *****************".
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-1.
           05 FILLER                    PIC X(36) VALUE
                              "Total amount claimed  : ".
           05 TOT-CLAIM-AMOUNT-OUT      PIC $$$,$$$,$$$.99CR.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-2.
           05 FILLER                    PIC X(36) VALUE
                              "Total claimed paid   : ".
           05 TOT-CLAIM-PAID-OUT        PIC $$$,$$$,$$$.99CR.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-3.
           05 FILLER                    PIC X(36) VALUE
                              "Total claims processed: ".
           05 TOT-CLAIMS-PRCSD-OUT      PIC ZZ9.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-4.
           05 FILLER                    PIC X(36) VALUE
                              "Total claims rejected: ".
           05 TOT-CLAIMS-RJCTD-OUT      PIC ZZ9.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES.
           MOVE SPACES TO  EXCP-REPORT-REC CLAIM-REPORT-REC.
           MOVE   'N' TO EOF-STAT.
           READ    CLAIMS-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
           PERFORM DISPLAY-HEADINGS.
           PERFORM EXCEPTION-HEADINGS.
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
           PERFORM DISPLAY-FOOTERS.
           PERFORM CLOSE-FILES.
           GOBACK.
      *
       PROCESS-RECORDS.
      *
           PERFORM VALIDATE-RECORD-READ.
      *
      *
           IF NOT WRITE-EXCEPTION
              PERFORM PREPARE-CLAIM-REPORT-DATA.
      *
           IF NOT WRITE-EXCEPTION
              PERFORM VALIDATE-DEDUCT-AND-CLAIM-AMT.
      *
           IF NOT WRITE-EXCEPTION
              PERFORM PROCESS-CLAIM-AMT.
      *
           IF WRITE-EXCEPTION
              PERFORM  WRITE-EXCEPTION-RECORD
              ADD 1                 TO    TOT-CLAIMS-RJCTD
           ELSE
               WRITE CLAIM-REPORT-REC  AFTER ADVANCING 2 LINES
               ADD 1                 TO TOT-CLAIMS-PRCSD
               ADD CLAIM-AMOUNT      TO TOT-CLAIM-AMOUNT
               ADD CLAIM-AMOUNT-PAID TO TOT-CLAIM-PAID
           END-IF.
           MOVE SPACES TO  CLAIM-REPORT-REC  EXCP-REPORT-REC.
           MOVE SPACE TO EXCPTN-STAT.
           READ  CLAIMS-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
       VALIDATE-RECORD-READ.
      *
           IF INSURED-POLICY-NO NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC POLICY #' TO EXCP-REASON.
      *
           IF INSURED-LAST-NAME NOT ALPHABETIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-ALPHA LASTNAME' TO EXCP-REASON.
      *
           IF INSURED-FIRST-NAME NOT ALPHABETIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-ALPHA FIRSTNAME' TO EXCP-REASON.
      *
           IF POLICY-TYPE  NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC POLICY TYPE' TO EXCP-REASON.
      *
           IF POLICY-BENEFIT-DATE-NUM NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC POLICY DATE' TO EXCP-REASON.
      *
           IF POLICY-AMOUNT NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC POLICY AMOUNT' TO EXCP-REASON.
      *
           IF POLICY-DEDUCTIBLE-PAID NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC POLICY DEDUCTIBLE' TO EXCP-REASON.
      *
           IF POLICY-COINSURANCE NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC COINSURANCE' TO EXCP-REASON.
      *
           IF CLAIM-AMOUNT NOT NUMERIC
              MOVE 'Y' TO EXCPTN-STAT
              MOVE 'NON-NUMERIC CLAIM AMOUNT' TO EXCP-REASON.
      *
       PREPARE-CLAIM-REPORT-DATA.
      *
           MOVE INSURED-POLICY-NO        TO INSURED-POLICY-NO-OUT.
           MOVE INSURED-LAST-NAME        TO INSURED-LASTNAME-OUT.
           MOVE INSURED-FIRST-NAME       TO INSURED-FIRSTNAME-OUT.
      *
           MOVE POLICY-AMOUNT            TO POLICY-AVAIL-AMT-OUT.
           MOVE POLICY-COINSURANCE       TO POLICY-COPAY-OUT.
           MOVE POLICY-DEDUCTIBLE-PAID   TO POLICY-DED-OUT.
           MOVE CLAIM-AMOUNT             TO CLAIM-AMT-OUT.
      *
           EVALUATE TRUE
              WHEN PRIVATE  MOVE 'EMPLOYER PRIVATE'  TO POLICY-TYPE-DESC

              WHEN MEDICARE MOVE 'STANDARD MEDICARE' TO POLICY-TYPE-DESC
              WHEN AFFORDABLE-CARE MOVE 'AFFORDABLE CARE ACT'
                                                  TO POLICY-TYPE-DESC
           END-EVALUATE.
      *
           MOVE POLICY-BENEFIT-DATE-X     TO POLICY-DATE-OUT.
      *
           PERFORM DETERMINE-RENEWAL-DATE.
      *
      *
       VALIDATE-DEDUCT-AND-CLAIM-AMT.
           EVALUATE TRUE
              WHEN  PRIVATE
                  IF POLICY-DEDUCTIBLE-PAID >= MAX-DED-AMT-PRIVATE
                     MOVE 'Y' TO  POLICY-DEDUCT-PD-OUT
                  ELSE
                     MOVE 'N' TO  POLICY-DEDUCT-PD-OUT
                  END-IF
                  IF CLAIM-AMOUNT >  MAX-COVER-AMT-PRIVATE
                     MOVE 'CLAIM EXCEED MAX COVERAGE' TO EXCP-REASON
                     MOVE 'Y' TO EXCPTN-STAT
                  END-IF
              WHEN  MEDICARE
                  IF POLICY-DEDUCTIBLE-PAID >= MAX-DED-AMT-MEDICARE
                     MOVE 'Y' TO  POLICY-DEDUCT-PD-OUT
                  ELSE
                     MOVE 'N' TO  POLICY-DEDUCT-PD-OUT
                  END-IF
                  IF CLAIM-AMOUNT >  MAX-COVER-AMT-MEDICARE
                     MOVE 'CLAIM EXCEED MAX COVERAGE' TO EXCP-REASON
                     MOVE 'Y' TO EXCPTN-STAT
                  END-IF
              WHEN  AFFORDABLE-CARE
                  IF POLICY-DEDUCTIBLE-PAID >= MAX-DED-AMT-AFFORDABLE
                     MOVE 'Y' TO  POLICY-DEDUCT-PD-OUT
                  ELSE
                     MOVE 'N' TO  POLICY-DEDUCT-PD-OUT
                  END-IF
                  IF CLAIM-AMOUNT >  MAX-COVER-AMT-AFFORDABLE
                     MOVE 'CLAIM EXCEED MAX COVERAGE' TO EXCP-REASON
                     MOVE 'Y' TO EXCPTN-STAT
                  END-IF
           END-EVALUATE.
      *
      *--------------------------------------------------------------*
      *    Now, check if the claim exceeds the amount left on the    *
      *         policy.  Deny the claim.                             *
      *--------------------------------------------------------------*
      *
           IF CLAIM-AMOUNT > POLICY-AMOUNT
              MOVE 'CLAIM EXCEEDS POLICY BALANCE' TO EXCP-REASON
              MOVE 'Y'  TO EXCPTN-STAT
           END-IF.
      *
       PROCESS-CLAIM-AMT.
      *
      * -------------------------------------------------------------*
      *   If deductible has been met,
      *      Claim-amt-paid = Claim-amt -
      *                      (Claim-amt * Policy-coinsurance).
      *
      *   However, if claim-amt has not been met,
      *      Claim-amt-paid = Claim-amt - deductible -
      *                      (Claim-amt * Policy-coinsurance).
      *--------------------------------------------------------------*
      *
           IF DEDUCTIBLE-PAID
              COMPUTE CLAIM-AMOUNT-PAID =  CLAIM-AMOUNT -
                      (CLAIM-AMOUNT * POLICY-COINSURANCE)
           ELSE
              EVALUATE TRUE
                  WHEN PRIVATE
                       COMPUTE CLAIM-AMOUNT-PAID = CLAIM-AMOUNT -
                               MAX-DED-AMT-PRIVATE -
                              (POLICY-COINSURANCE * CLAIM-AMOUNT)
                  WHEN MEDICARE
                       COMPUTE CLAIM-AMOUNT-PAID = CLAIM-AMOUNT -
                               MAX-DED-AMT-MEDICARE -
                              (POLICY-COINSURANCE * CLAIM-AMOUNT)
                  WHEN AFFORDABLE-CARE
                       COMPUTE CLAIM-AMOUNT-PAID = CLAIM-AMOUNT -
                               MAX-DED-AMT-AFFORDABLE -
                              (POLICY-COINSURANCE * CLAIM-AMOUNT)
              END-EVALUATE
           END-IF.
      *
           MOVE CLAIM-AMOUNT-PAID  TO CLAIM-PAID-OUT.
      *
      *
       DETERMINE-RENEWAL-DATE.
      *
      *--------------------------------------------------------------*
      *    Code below uses Intrinsic Function to add a year
      *         to the Policy Date to get the Renewal Date.
      *    Granting that benefit period is on a yearly basis.
      *--------------------------------------------------------------*
      *
      *
           COMPUTE INT-POLICY-RENEWAL-DATE =
                   FUNCTION INTEGER-OF-DATE(POLICY-BENEFIT-DATE-NUM)
                           + 365.
           COMPUTE WS-RENEWAL-DATE =
                   FUNCTION DATE-OF-INTEGER(INT-POLICY-RENEWAL-DATE).
      *
           IF  WS-RENEWAL-DATE <  WS-CURRENT-DATE
                     MOVE 'COVERAGE IS EXPIRED' TO EXCP-REASON
                     MOVE 'Y' TO EXCPTN-STAT.
      *
            MOVE  WS-RENEW-DATE-REDEF  TO  POLICY-RENEW-DATE-OUT.
      *
       DISPLAY-HEADINGS.
           MOVE  FUNCTION  CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE  WS-CURRENT-YEAR        TO WS-CURR-YEAROUT.
           MOVE  WS-CURRENT-MONTH       TO WS-CURR-MONTHOUT.
           MOVE  WS-CURRENT-DAY         TO WS-CURR-DAYOUT.
      *
           MOVE  SPACES     TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           MOVE  SPACES     TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM   HEADER-LINE-1
                            AFTER ADVANCING 3 LINES.
           MOVE  SPACES     TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM   HEADER-LINE-2
                            AFTER ADVANCING 3 LINES.

           MOVE  SPACES     TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM   HEADER-LINE-3
                            AFTER ADVANCING 1 LINE.
           MOVE  SPACES     TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC  AFTER ADVANCING 2 LINES.
      *
      *
       DISPLAY-FOOTERS.
           MOVE  TOT-CLAIMS-RJCTD   TO TOT-CLAIMS-RJCTD-OUT.
           MOVE  TOT-CLAIMS-PRCSD   TO TOT-CLAIMS-PRCSD-OUT.
           MOVE  TOT-CLAIM-AMOUNT   TO TOT-CLAIM-AMOUNT-OUT.
           MOVE  TOT-CLAIM-PAID     TO TOT-CLAIM-PAID-OUT.
           MOVE  SPACES       TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM  FOOTER-LINE-0.
           MOVE  SPACES       TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM  FOOTER-LINE-1.
           MOVE  SPACES       TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM  FOOTER-LINE-2.
           MOVE  SPACES       TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM  FOOTER-LINE-3.
           MOVE  SPACES       TO     CLAIM-REPORT-REC.
           WRITE CLAIM-REPORT-REC   FROM  FOOTER-LINE-4.
      *
       EXCEPTION-HEADINGS.
           MOVE  '                   EXCEPTION REPORT ON MEDICAL CLAIMS'
                              TO EXCP-CLAIM-RECORD.
           WRITE EXCP-REPORT-REC.
           MOVE SPACES TO EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.
           MOVE  'MEDICAL CLAIMS'       TO EXCP-CLAIM-RECORD.
           MOVE  'REASON FOR REJECTING' TO EXCP-REASON.
           WRITE EXCP-REPORT-REC.
           MOVE  '=============='       TO EXCP-CLAIM-RECORD.
           MOVE  '====================' TO EXCP-REASON.
           WRITE EXCP-REPORT-REC.
           MOVE  SPACES TO EXCP-REPORT-REC.
           WRITE EXCP-REPORT-REC.

       WRITE-EXCEPTION-RECORD.
           MOVE  CLAIM-RECORD-WS     TO EXCP-CLAIM-RECORD.
           WRITE EXCP-REPORT-REC     AFTER ADVANCING 2 LINES.
      *
      *
       OPEN-FILES.
           OPEN INPUT  CLAIMS-FILE.
           OPEN OUTPUT CLAIMS-REPORT EXCEPTION-REPORT.
      *
       CLOSE-FILES.
           CLOSE CLAIMS-FILE.
           CLOSE CLAIMS-REPORT EXCEPTION-REPORT.