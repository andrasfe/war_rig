
           MOVE PENDING-AUTH-DETAILS        TO WS-FRAUD-AUTH-RECORD
           MOVE CDEMO-ACCT-ID               TO WS-FRD-ACCT-ID
           MOVE CDEMO-CUST-ID               TO WS-FRD-CUST-ID

           EXEC CICS LINK
                PROGRAM(WS-PGM-AUTH-FRAUD)
                COMMAREA(WS-FRAUD-DATA)
                NOHANDLE
