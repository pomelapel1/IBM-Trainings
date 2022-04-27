       IDENTIFICATION DIVISION.
       PROGRAM-ID. POEDIT.
       AUTHOR. NANCY SLOCUM.
      *
      **********    Final Project    *********************************
      *   POREDIT Subroutine is called to validate the PURCHASE
      *   section of Transaction Records as per the following
      *   specifications:
      *
      *   The following fields are required.
      *   a)  PO Number
      *   b)  Buyer Code
      *   c)  Order Date
      *   d)  Quantity
      *
      *  CRITERIA:
      *   - Quantity must be between 0 and 999,999
      *   - If Quantity is 0, Unit-Price must also be 0
      *   - If > 0, Unit Price must be between $1 and $1,000,000.00
      *   - Order date must be a valid date
      *   - Delivery date is optional but if it exists, it must be a
      *     valid date later than the order date.
      *
      *  Dates validated with CEEDAYS via the CHECK-DATE paragraph.
      *
      *   Subroutine returns the following:
      *   a)  Total field validation errors in ERROR-CTR
      *   b)  A short description of the error in validation.
      *
      *   PREPARE-EXCP-DESC paragraph, prepares, concatenates
      *   all probable short description errors identified during
      *   field validations.
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Invalid field strings
       01  INVALID-PO-DESC                PIC X(30) VALUE SPACES.
           88 PO-STR          VALUE 'PO NUMBER '.
           88 BUYER-STR       VALUE 'BUYER CODE '.
           88 QUANTITY-STR    VALUE 'QUANTITY '.
           88 PRICE-STR       VALUE 'UNIT PRICE '.
           88 ORDER-STR       VALUE 'ORDER DATE '.
           88 DELIVERY-STR    VALUE 'DELIVERY DATE '.
      *Date checking
       01  DATE-TO-CHECK              PIC 9(8).
       01  INV-DATE-FLAG              PIC X.
           88 INV-DATE                VALUE 'Y'.
       01  WS-PO-ACT-DATE             PIC 9(9) COMP.
       01  WS-PICSTR-IN.
           05  WS-PICSTR-LTH-IN             PIC S9(4) COMP VALUE 8.
           05  WS-PICSTR-STR-IN             PIC X(8) VALUE 'YYYYMMDD'.
       01  WS-DATE-IN-CEE.
           05  WS-DATE-IN-LTH-CEE           PIC S9(4) COMP VALUE 8.
           05  WS-DATE-IN-STR-CEE           PIC X(8).
       01  FC.
           05  FC-SEV                       PIC S9(4) COMP.
           05  FC-MSG                       PIC S9(4) COMP.
           05  FC-CTW                       PIC X.
           05  FC-FAC                       PIC X(3).
           05  FC-ISI                       PIC S9(8) COMP.
      *
       LINKAGE SECTION.
       01  DATA-RECEIVED.
           05  VLDT-PO                    PIC X(6).
               88 INV-PO         VALUES  ZEROES, SPACES.
           05  VLDT-BUYER                 PIC X(3).
               88 INV-BUYER      VALUES  ZEROES, SPACES.
           05  VLDT-QUANTITY              PIC S9(7).
           05  VLDT-PRICE                 PIC S9(7)V99.
           05  VLDT-ORDER                 PIC 9(8).
               88 INV-ORDER      VALUES  ZEROES, SPACES.
           05  VLDT-DELIVERY              PIC 9(8).
           05  VALID-PO                   PIC 9.
       77  ERROR-CTR                      PIC 9.
       77  EXCP-DESC                      PIC X(50).
      *
       PROCEDURE DIVISION USING DATA-RECEIVED, ERROR-CTR,
                                EXCP-DESC.
       MAIN-RTN.
           PERFORM VALIDATE-PO-FIELDS.
      *  Determines whether PO will be printed in the main report
           IF ERROR-CTR > 0
              MOVE 8 TO VALID-PO
           ELSE
              MOVE 0 TO VALID-PO
           END-IF.
      *
           GOBACK.
      *
       VALIDATE-PO-FIELDS.
           MOVE SPACES TO EXCP-DESC.
      *  Test PO Number
           IF INV-PO
               SET PO-STR TO TRUE
               PERFORM PREPARE-EXCP-DESC
               ADD 1 TO ERROR-CTR
           END-IF.
      *  Test Buyer Code
           IF INV-BUYER
               SET BUYER-STR TO TRUE
               PERFORM PREPARE-EXCP-DESC
               ADD 1 TO ERROR-CTR
           END-IF.
      *  Test Quantity
           IF VLDT-QUANTITY EQUAL SPACES OR  VLDT-QUANTITY > 999999
               SET QUANTITY-STR TO TRUE
               PERFORM PREPARE-EXCP-DESC
               ADD 1 TO ERROR-CTR
           END-IF.
      *  Test Unit Price, depending on Quantity value
           EVALUATE VLDT-QUANTITY ALSO VLDT-PRICE
              WHEN SPACES ALSO NOT 0 *> No quantity, unit exists
              WHEN NOT 0 ALSO SPACES *> Quantity exists, no unit
              WHEN 0 ALSO NOT 0 *> Quantity is 0, Unit is not 0
              WHEN NOT 0 ALSO 0 *> Quantity is not 0, Unit is 0
              WHEN NOT 0 ALSO > 1000000 *> Quantity > 0, Unit > 1 mil
                 SET PRICE-STR TO TRUE
                 PERFORM PREPARE-EXCP-DESC
                 ADD 1 TO ERROR-CTR
           END-EVALUATE.
      *  Test order date
           MOVE VLDT-ORDER TO DATE-TO-CHECK.
           PERFORM CHECK-DATE.
           IF INVALID-DATE
             SET ORDER-STR TO TRUE
             PERFORM PREPARE-EXCP-DESC
           END-IF.
      *  Check whether delivery date exists
           IF VLDT-DELIVERY NOT SPACES
              PERFORM VALIDATE-DELIVERY
           END-IF.
       VALIDATE-DELIVERY.
           IF VLDT-DELIVERY > VLDT-ORDER
              MOVE VLDT-DELIVERY TO DATE-TO-CHECK
              PERFORM CHECK-DATE
              IF INVALID-DATE
                 CONTINUE
              ELSE
                 NEXT SENTENCE
              END-IF
           ELSE
              SET DELIVERY-STR TO TRUE
              PERFORM PREPARE-EXCP-DESC
           END-IF.
      *  Write string to exception description
       PREPARE-EXCP-DESC.
           IF  EXCP-DESC EQUAL SPACES
               STRING       'INVALID ' DELIMITED BY SIZE
               INVALID-PO-DESC DELIMITED BY '  '
               INTO EXCP-DESC
           ELSE
               STRING  EXCP-DESC DELIMITED BY '  '
               '/ '    DELIMITED BY SIZE
               INVALID-PO-DESC DELIMITED BY '  '
               INTO EXCP-DESC
           END-IF.
           DISPLAY EXCP-DESC.
      *  Validate a given date string using CEEDAYS
       CHECK-DATE.
           MOVE  DATE-TO-CHECK TO  WS-DATE-IN-STR-CEE.
           DISPLAY ' DATE IN  ' WS-DATE-IN-CEE
           CALL 'CEEDAYS' USING WS-DATE-IN-CEE, WS-PICSTR-IN,
                                WS-PO-ACT-DATE, FC.
           IF FC-SEV NOT = ZERO
              DISPLAY ' BAD DATE ' WS-PO-ACT-DATE
              SET INVALID-DATE TO TRUE
              ADD 1 TO ERROR-CTR
           ELSE
              DISPLAY ' GOOD DATE '  WS-PO-ACT-DATE
           END-IF.