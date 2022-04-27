       01 SUPPLIER-REC.
           05  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
           05  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                88 VALID-SUPPLIER-TYPES VALUES 'S', 'D', 'M', 'I'.
                88 SUBCONTRACTOR  VALUE 'S'.
                88 DISTRIBUTOR    VALUE 'D'.
                88 MANUFACTURER   VALUE 'M'.
                88 IMPORTER       VALUE 'I'.
           05  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05  SUPPLIER-PERF     PIC 9(03) COMP VALUE ZERO.
           05  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                88 VALID-SUPPLIER-RATING     VALUES '3', '2', '1'.
                88 HIGHEST-QUALITY VALUE '3'.
                88 AVERAGE-QUALITY VALUE '2'.
                88 LOWEST-QUALITY  VALUE '1'.
           05  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                88 VALID-SUPPLIER-STATUS     VALUES '1', '2', '3'.
                88 GOVT-COMM       VALUE '1'.
                88 GOVT-ONLY       VALUE '2'.
                88 COMMERCIAL-ONLY VALUE '3'.
           05  SUPPLIER-ACT-DATE PIC X(08) VALUE SPACES.