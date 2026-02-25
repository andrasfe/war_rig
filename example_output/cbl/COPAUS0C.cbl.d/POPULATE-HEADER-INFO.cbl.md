```cobol
       POPULATE-HEADER-INFO.
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COPAU0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COPAU0AO
           MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU0AO
           MOVE WS-PGM-AUTH-SMRY       TO PGMNAMEO OF COPAU0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU0AO
           .

      *****************************************************************
       GATHER-ACCOUNT-DETAILS.
      *****************************************************************

           PERFORM GETCARDXREF-BYACCT
           PERFORM GETACCTDATA-BYACCT
           PERFORM GETCUSTDATA-BYCUST

           MOVE CUST-ID                TO CUSTIDO
           STRING CUST-FIRST-NAME DELIMITED BY SPACES
                  ' ' DELIMITED BY SIZE
                  CUST-MIDDLE-NAME(1:1) DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  CUST-LAST-NAME DELIMITED BY SPACES
                  INTO CNAMEO
           END-STRING

           STRING CUST-ADDR-LINE-1 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-LINE-2 DELIMITED BY '  '
                  INTO ADDR001O
           END-STRING
           STRING CUST-ADDR-LINE-3 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-STATE-CD DELIMITED BY SIZE
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-ZIP(1:5) DELIMITED BY SIZE
                  INTO ADDR002O
           END-STRING

           MOVE CUST-PHONE-NUM-1       TO PHONE1O
           MOVE ACCT-CREDIT-LIMIT      TO WS-DISPLAY-AMT12
           MOVE WS-DISPLAY-AMT12       TO CREDLIMO
           MOVE ACCT-CASH-CREDIT-LIMIT TO WS-DISPLAY-AMT9
           MOVE WS-DISPLAY-AMT9        TO CASHLIMO

           PERFORM GET-AUTH-SUMMARY

           IF FOUND-PAUT-SMRY-SEG
              MOVE PA-APPROVED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO APPRCNTO
              MOVE PA-DECLINED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO DECLCNTO
              MOVE PA-CREDIT-BALANCE      TO WS-DISPLAY-AMT12
              MOVE WS-DISPLAY-AMT12       TO CREDBALO
              MOVE PA-CASH-BALANCE        TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO CASHBALO
              MOVE PA-APPROVED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO APPRAMTO
              MOVE PA-DECLINED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO DECLAMTO
           ELSE
              MOVE ZERO                   TO APPRCNTO
                                             DECLCNTO
                                             CREDBALO
                                             CASHBALO
                                             APPRAMTO
                                             DECLAMTO
           END-IF
           .


      *****************************************************************
       GETCARDXREF-BYACCT.
      *****************************************************************

      *    Read the Card file. Access via alternate index ACCTID
      *
           MOVE WS-ACCT-ID          TO WS-CARD-RID-ACCT-ID-X
           EXEC CICS READ
                DATASET   (WS-CARDXREFNAME-ACCT-PATH)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  MOVE XREF-CUST-ID               TO CDEMO-CUST-ID
                  MOVE XREF-CARD-NUM              TO CDEMO-CARD-NUM
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-ACCT-ID
                  ' not found in XREF file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading XREF file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETACCTDATA-BYACCT.
      *****************************************************************

           MOVE XREF-ACCT-ID            TO WS-CARD-RID-ACCT-ID
           EXEC CICS READ
                DATASET   (WS-ACCTFILENAME)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  continue
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in ACCT file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading ACCT file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETCUSTDATA-BYCUST.
      *****************************************************************

           MOVE XREF-CUST-ID              TO WS-CARD-RID-CUST-ID

           EXEC CICS READ
                DATASET   (WS-CUSTFILENAME)
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  CONTINUE
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' not found in CUST file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' System error while reading CUST file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GET-AUTH-SUMMARY.
      *****************************************************************

           PERFORM SCHEDULE-PSB

           MOVE CDEMO-ACCT-ID                   TO PA-ACCT-ID
      *    MOVE XREF-ACCT-ID                    TO PA-ACCT-ID
           EXEC DLI GU USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTSUM0)
               INTO (PENDING-AUTH-SUMMARY)
               WHERE (ACCNTID = PA-ACCT-ID)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET FOUND-PAUT-SMRY-SEG        TO TRUE
               WHEN SEGMENT-NOT-FOUND
                  SET NFOUND-PAUT-SMRY-SEG       TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading AUTH Summary: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .
      *****************************************************************
      * SCHEDULE PSB                                                  *
      *****************************************************************
       SCHEDULE-PSB.
           EXEC DLI SCHD
                PSB((PSB-NAME))
                NODHABEND
           END-EXEC
           MOVE DIBSTAT        TO IMS-RETURN-CODE
           IF PSB-SCHEDULED-MORE-THAN-ONCE
              EXEC DLI TERM
              END-EXEC

              EXEC DLI SCHD
                   PSB((PSB-NAME))
                   NODHABEND
              END-EXEC
              MOVE DIBSTAT     TO IMS-RETURN-CODE
           END-IF
           IF STATUS-OK
              SET IMS-PSB-SCHD           TO TRUE
           ELSE
              MOVE 'Y'     TO WS-ERR-FLG

              STRING
                  ' System error while scheduling PSB: Code:'
              IMS-RETURN-CODE
              DELIMITED BY SIZE
              INTO WS-MESSAGE
              END-STRING
              MOVE -1       TO ACCTIDL OF COPAU0AI
              PERFORM SEND-PAULST-SCREEN
           END-IF
           .
```
