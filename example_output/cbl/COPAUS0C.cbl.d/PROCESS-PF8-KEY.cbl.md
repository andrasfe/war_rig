```cobol
                  SET AUTHS-EOF                  TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while repos. AUTH Details: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE

           .

      *****************************************************************
       POPULATE-AUTH-LIST.
      *****************************************************************

           MOVE PA-APPROVED-AMT           TO WS-AUTH-AMT

           MOVE PA-AUTH-ORIG-TIME(1:2)    TO WS-AUTH-TIME(1:2)
           MOVE PA-AUTH-ORIG-TIME(3:2)    TO WS-AUTH-TIME(4:2)
           MOVE PA-AUTH-ORIG-TIME(5:2)    TO WS-AUTH-TIME(7:2)

           MOVE PA-AUTH-ORIG-DATE(1:2)    TO WS-CURDATE-YY
           MOVE PA-AUTH-ORIG-DATE(3:2)    TO WS-CURDATE-MM
           MOVE PA-AUTH-ORIG-DATE(5:2)    TO WS-CURDATE-DD
           MOVE WS-CURDATE-MM-DD-YY       TO WS-AUTH-DATE

           IF PA-AUTH-RESP-CODE = '00'
              MOVE 'A'               TO WS-AUTH-APRV-STAT
           ELSE
              MOVE 'D'               TO WS-AUTH-APRV-STAT
           END-IF

           EVALUATE WS-IDX
               WHEN 1
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(1)
```
