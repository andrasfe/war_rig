         05 WS-AUTH-TIME               PIC X(08) VALUE '00:00:00'.              
                                                                                
       01 WS-TABLES.
          05 WS-DECLINE-REASON-TABLE.
             10   PIC X(20) VALUE '0000APPROVED'.
             10   PIC X(20) VALUE '3100INVALID CARD'.
             10   PIC X(20) VALUE '4100INSUFFICNT FUND'.
             10   PIC X(20) VALUE '4200CARD NOT ACTIVE'.
             10   PIC X(20) VALUE '4300ACCOUNT CLOSED'.
             10   PIC X(20) VALUE '4400EXCED DAILY LMT'.
             10   PIC X(20) VALUE '5100CARD FRAUD'.
             10   PIC X(20) VALUE '5200MERCHANT FRAUD'.
             10   PIC X(20) VALUE '5300LOST CARD'.
             10   PIC X(20) VALUE '9000UNKNOWN'.
          05 WS-DECLINE-REASON-TAB REDEFINES WS-DECLINE-REASON-TABLE
                                OCCURS 10 TIMES
                                ASCENDING KEY IS DECL-CODE
                                INDEXED BY WS-DECL-RSN-IDX.
             10 DECL-CODE                PIC X(4).
             10 DECL-DESC                PIC X(16).

