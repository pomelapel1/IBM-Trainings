       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MORTGAGE.
       ENVIRONMENT DIVISION.
      *******************************************************
      *    Debugging the Mortgage program   *
      *******************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FACTORS-WS.
           05  PRINCIPAL                 PIC 9(07)V99 VALUE 100000.00.
           05  INT-RATE                  PIC 9(2)v9(6).
           05  NBR-OF-PAYMENTS           PIC 999      VALUE 360.
           05  MONTHLY-PAYMENT           PIC -9V9(9)E-99.
           05  INT-FORMULA               PIC -9V9(9)E-99.
      *
       01  FACTORS-PRINTOUT.
           05  PRINCIPAL-OUT             PIC $$,$$$,$$$.99.
           05  INT-RATE-OUT              PIC ***.999999.
           05  MONTHLY-PAYMENT-OUT       PIC $$$,$$$,$$$.99.
           05  INT-FORMULA-OUT           PIC $$$,$$$,$$$.99.
      *
       PROCEDURE DIVISION.
           PERFORM COMPUTE-PYMT-MANUALLY.
           DISPLAY "RESULT FOR MANUAL COMPUTATION. ".
           PERFORM DISPLAY-RESULTS.
           PERFORM COMPUTE-PYMT-WITH-FUNCTION.
           DISPLAY "RESULT FOR COMPUTATIION USING INTRINSIC FUNCTION ".
           PERFORM DISPLAY-RESULTS.
           GOBACK.
      *
       COMPUTE-PYMT-MANUALLY.
           PERFORM INIT-VARIABLES.
           DISPLAY "TESTS FOR MANUAL COMPUTATION".
           COMPUTE INT-RATE = ((03 / 100) / 12).
           DISPLAY "INTEREST RATE " INT-RATE.
           MOVE INT-RATE           TO INT-RATE-OUT.
           DISPLAY "INTEREST RATE FORMATTED" INT-RATE-OUT.
      *************************************************************
      *    TRYING TO SEE THE VALUE OF THIS EQUATION BELOW:
      *          ((1 + INT-RATE ) ** NBR-OF-PAYMENTS)
      *
      *************************************************************
           COMPUTE INT-FORMULA = ((1 + INT-RATE ) ** NBR-OF-PAYMENTS).
           DISPLAY "INTERMEDIATE FORMULA RESULT ".
           DISPLAY " ((1 + INT-RATE ) ** NBR-OF-PAYMENTS) " INT-FORMULA.
           MOVE    INT-FORMULA     TO  INT-FORMULA-OUT .
           DISPLAY "INTERMEDIATE FORMULA FORMATTED " INT-FORMULA-OUT.
           DISPLAY "  ".
           DISPLAY "  ".
      *
      **************************************************************
      *     NEXT TRYING TO SEE THE VALUE OF THIS EQUATION BELOW:
      *         (((1 + INT-RATE ) ** NBR-OF-PAYMENTS) - 1)
      *
      *************************************************************
           COMPUTE INT-FORMULA =
               (((1 + INT-RATE ) ** NBR-OF-PAYMENTS) - 1).
           DISPLAY "INTERMEDIATE FORMULA RESULT ".
           DISPLAY " (((1 + INT-RATE ) ** NBR-OF-PAYMENTS) - 1) "
                   INT-FORMULA.
           MOVE    INT-FORMULA     TO  INT-FORMULA-OUT .
           DISPLAY "INTERMEDIATE FORMULA FORMATTED " INT-FORMULA-OUT.
           DISPLAY "  ".
           DISPLAY "  ".
      *
           COMPUTE MONTHLY-PAYMENT
                     = PRINCIPAL *
                        (INT-RATE *
                    (1 + INT-RATE) ** NBR-OF-PAYMENTS) /
                  (((1 + INT-RATE ) ** NBR-OF-PAYMENTS) - 1).

       COMPUTE-PYMT-WITH-FUNCTION.
           MOVE .03 TO INT-RATE.
           COMPUTE MONTHLY-PAYMENT = PRINCIPAL *
               FUNCTION ANNUITY((INT-RATE / 12) NBR-OF-PAYMENTS).
      *
       DISPLAY-RESULTS.
           MOVE     PRINCIPAL         TO PRINCIPAL-OUT.
           MOVE     INT-RATE          TO INT-RATE-OUT.
           MOVE     MONTHLY-PAYMENT   TO MONTHLY-PAYMENT-OUT.
           DISPLAY "                                 ".
           DISPLAY  "THE MONTHLY PAYMENT REQUIRED IS "
                    MONTHLY-PAYMENT-OUT.
           DISPLAY  "FOR THE PRINCIPAL AMOUNT OF, " PRINCIPAL-OUT.
           DISPLAY  "WITH THE INTEREST RATE OF " INT-RATE-OUT.
           DISPLAY  "FOR " NBR-OF-PAYMENTS  " NUMBER OF PAYMENTS. ".
           DISPLAY "                                 ".
      *
       INIT-VARIABLES.
           MOVE ZEROES TO INT-RATE INT-FORMULA MONTHLY-PAYMENT
                INT-RATE-OUT  INT-FORMULA-OUT MONTHLY-PAYMENT-OUT.
