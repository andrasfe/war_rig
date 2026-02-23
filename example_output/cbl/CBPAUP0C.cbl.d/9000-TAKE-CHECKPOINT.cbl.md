```cobol
      *
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.
      *
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.
           88 END-OF-AUTHDB                      VALUE 'Y'.
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.
           88 MORE-AUTHS                         VALUE 'Y'.
           88 NO-MORE-AUTHS                      VALUE 'N'.
         05 WS-QUALIFY-DELETE-FLAG     PIC X(01) VALUE 'N'.
           88 QUALIFIED-FOR-DELETE               VALUE 'Y'.
           88 NOT-QUALIFIED-FOR-DELETE           VALUE 'N'.
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.
```
