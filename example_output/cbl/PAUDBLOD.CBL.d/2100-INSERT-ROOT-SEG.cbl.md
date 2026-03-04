```cobol
           88 END-OF-AUTHDB                      VALUE 'Y'.             00490026
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.             00500026
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.             00510026
           88 MORE-AUTHS                         VALUE 'Y'.             00520026
           88 NO-MORE-AUTHS                      VALUE 'N'.             00530026
         05 WS-END-OF-INFILE1          PIC X(01) VALUE SPACES.          00540053
         05 WS-END-OF-INFILE2          PIC X(01) VALUE SPACES.          00550053
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.          00570026
         05 WS-INFIL1-STATUS           PIC X(02) VALUE SPACES.          00571053
         05 WS-INFIL2-STATUS           PIC X(02) VALUE SPACES.          00572053
         05 END-ROOT-SEG-FILE          PIC X(01) VALUE SPACES.          00573053
         05 END-CHILD-SEG-FILE         PIC X(01) VALUE SPACES.          00574053
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.          00580026
            88 END-OF-FILE                       VALUE '10'.            00590026
      *                                                                 00600026
         05 WK-CHKPT-ID.                                                00610026
            10  FILLER              PIC  X(04) VALUE 'RMAD'.            00620026
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.            00630026
      *                                                                 00640026
       01 WS-IMS-VARIABLES.                                             00650026
      *   05 PSB-NAME                        PIC X(8) VALUE 'IMSUNLOD'. 00660042
      *   05 PCB-OFFSET.                                                00670042
```
