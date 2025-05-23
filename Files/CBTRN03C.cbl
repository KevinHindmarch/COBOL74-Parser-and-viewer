      ******************************************************************
      * Program     : CBTRN03C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Print the transaction detail report.     
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.                                          
       PROGRAM-ID. CBTRN03C.                                            
       AUTHOR. AWS.                                                     
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE                      
                  ORGANIZATION IS SEQUENTIAL                            
                  FILE STATUS  IS TRANFILE-STATUS.                      
                                                                        
           SELECT XREF-FILE ASSIGN TO CARDXREF                          
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-XREF-CARD-NUM                      
                  FILE STATUS  IS CARDXREF-STATUS.                      
                                                                        
           SELECT TRANTYPE-FILE ASSIGN TO TRANTYPE                      
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-TRAN-TYPE                          
                  FILE STATUS  IS TRANTYPE-STATUS.                      
                                                                        
           SELECT TRANCATG-FILE ASSIGN TO TRANCATG                      
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-TRAN-CAT-KEY                       
                  FILE STATUS  IS TRANCATG-STATUS.                      
                                                                        
           SELECT REPORT-FILE ASSIGN TO TRANREPT                        
                  ORGANIZATION IS SEQUENTIAL                            
                  FILE STATUS  IS TRANREPT-STATUS.                      
                                                                        
           SELECT DATE-PARMS-FILE ASSIGN TO DATEPARM                    
                  ORGANIZATION IS SEQUENTIAL                            
                  FILE STATUS  IS DATEPARM-STATUS.                      
      *                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  TRANSACT-FILE.                                               
       01 FD-TRANFILE-REC.                                              
          05 FD-TRANS-DATA      PIC X(304).                             
          05 FD-TRAN-PROC-TS    PIC X(26).                              
          05 FD-FILLER          PIC X(20).                              
                                                                        
       FD  XREF-FILE.                                                   
       01  FD-CARDXREF-REC.                                             
           05 FD-XREF-CARD-NUM                  PIC X(16).              
           05 FD-XREF-DATA                      PIC X(34).              
                                                                        
       FD  TRANTYPE-FILE.                                               
       01 FD-TRANTYPE-REC.                                              
          05 FD-TRAN-TYPE       PIC X(02).                              
          05 FD-TRAN-DATA       PIC X(58).                              
                                                                        
       FD  TRANCATG-FILE.                                               
       01 FD-TRAN-CAT-RECORD.                                           
           05  FD-TRAN-CAT-KEY.                                         
              10  FD-TRAN-TYPE-CD                         PIC X(02).    
              10  FD-TRAN-CAT-CD                          PIC 9(04).    
           05  FD-TRAN-CAT-DATA                           PIC X(54).    
                                                                        
       FD  REPORT-FILE.                                                 
       01 FD-REPTFILE-REC       PIC X(133).                             
                                                                        
       FD  DATE-PARMS-FILE.                                             
       01 FD-DATEPARM-REC       PIC X(80).                              
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      *****************************************************************
       COPY CVTRA05Y.                                                   
       01 TRANFILE-STATUS.                                              
          05 TRANFILE-STAT1     PIC X.                                  
          05 TRANFILE-STAT2     PIC X.                                  
                                                                        
       COPY CVACT03Y.                                                   
       01  CARDXREF-STATUS.                                             
           05  CARDXREF-STAT1      PIC X.                               
           05  CARDXREF-STAT2      PIC X.                               
                                                                        
       COPY CVTRA03Y.                                                   
       01  TRANTYPE-STATUS.                                             
           05  TRANTYPE-STAT1      PIC X.                               
           05  TRANTYPE-STAT2      PIC X.                               
                                                                        
       COPY CVTRA04Y.                                                   
       01  TRANCATG-STATUS.                                             
           05  TRANCATG-STAT1      PIC X.                               
           05  TRANCATG-STAT2      PIC X.                               
                                                                        
       COPY CVTRA07Y.                                                   
       01 TRANREPT-STATUS.                                              
           05 REPTFILE-STAT1     PIC X.                                 
           05 REPTFILE-STAT2     PIC X.                                 
                                                                        
       01 DATEPARM-STATUS.                                              
           05 DATEPARM-STAT1     PIC X.                                 
           05 DATEPARM-STAT2     PIC X.                                 
                                                                        
       01 WS-DATEPARM-RECORD.                                           
           05 WS-START-DATE      PIC X(10).                             
           05 FILLER             PIC X(01).                             
           05 WS-END-DATE        PIC X(10).                             
                                                                        
       01 WS-REPORT-VARS.                                               
           05 WS-FIRST-TIME      PIC X      VALUE 'Y'.                  
           05 WS-LINE-COUNTER    PIC 9(09) COMP-3                       
                                            VALUE 0.                    
           05 WS-PAGE-SIZE       PIC 9(03) COMP-3                       
                                            VALUE 20.                   
           05 WS-BLANK-LINE      PIC X(133) VALUE SPACES.               
           05 WS-PAGE-TOTAL      PIC S9(09)V99 VALUE 0.                 
           05 WS-ACCOUNT-TOTAL   PIC S9(09)V99 VALUE 0.                 
           05 WS-GRAND-TOTAL     PIC S9(09)V99 VALUE 0.                 
           05 WS-CURR-CARD-NUM   PIC X(16) VALUE SPACES.                
                                                                        
       01 IO-STATUS.                                                    
          05 IO-STAT1           PIC X.                                  
          05 IO-STAT2           PIC X.                                  
       01 TWO-BYTES-BINARY      PIC 9(4) COMP.                          
       01 TWO-BYTES-ALPHA REDEFINES TWO-BYTES-BINARY.                   
          05 TWO-BYTES-LEFT     PIC X.                                  
          05 TWO-BYTES-RIGHT    PIC X.                                  
       01 IO-STATUS-04.                                                 
          05 IO-STATUS-0401     PIC 9      VALUE 0.                     
          05 IO-STATUS-0403     PIC 999    VALUE 0.                     
                                                                        
       01 APPL-RESULT           PIC S9(9) COMP.                         
          88 APPL-AOK                      VALUE 0.                     
          88 APPL-EOF                      VALUE 16.                    
                                                                        
       01 END-OF-FILE           PIC X(01)  VALUE 'N'.                   
       01 ABCODE                PIC S9(9) COMP.                         
       01 TIMING                PIC S9(9) COMP.                         
                                                                        
      *****************************************************************
       PROCEDURE DIVISION.                                              
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN03C'.            
           PERFORM 0000-TRANFILE-OPEN.                                  
           PERFORM 0100-REPTFILE-OPEN.                                  
           PERFORM 0200-CARDXREF-OPEN.                                  
           PERFORM 0300-TRANTYPE-OPEN.                                  
           PERFORM 0400-TRANCATG-OPEN.                                  
           PERFORM 0500-DATEPARM-OPEN.                                  
                                                                        
           PERFORM 0550-DATEPARM-READ.                                  
                                                                        
           PERFORM 1000-PROCESS-TRANSACTIONS                            
               UNTIL END-OF-FILE = 'Y'.                                 
                                                                        
           PERFORM 9000-TRANFILE-CLOSE.                                 
           PERFORM 9100-REPTFILE-CLOSE.                                 
           PERFORM 9200-CARDXREF-CLOSE.                                 
           PERFORM 9300-TRANTYPE-CLOSE.                                 
           PERFORM 9400-TRANCATG-CLOSE.                                 
           PERFORM 9500-DATEPARM-CLOSE.                                 
                                                                        
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN03C'.              
                                                                        
           GOBACK.                                                      
                                                                        
      * Read the date parameter file.                                   
       0550-DATEPARM-READ.                                              
           READ DATE-PARMS-FILE INTO WS-DATEPARM-RECORD.                
           IF DATEPARM-STATUS = '00'                                    
               MOVE 0 TO APPL-RESULT                                    
           ELSE                                                         
               IF DATEPARM-STATUS = '10'                                
                   MOVE 16 TO APPL-RESULT                               
               ELSE                                                     
                   MOVE 12 TO APPL-RESULT.                              
                                                                        
           IF APPL-AOK                                                  
              DISPLAY 'Reporting from ' WS-START-DATE                   
                 ' to ' WS-END-DATE                                     
           ELSE                                                         
              IF APPL-EOF                                               
                 MOVE 'Y' TO END-OF-FILE                                
              ELSE                                                      
                 DISPLAY 'ERROR READING DATEPARM FILE'                  
                 MOVE DATEPARM-STATUS TO IO-STATUS                      
                 PERFORM 9910-DISPLAY-IO-STATUS                         
                 PERFORM 9999-ABEND-PROGRAM.                            
                                                                        
      *****************************************************************
      * Process transactions                                          *
      *****************************************************************
       1000-PROCESS-TRANSACTIONS.                                       
           PERFORM 1000-TRANFILE-GET-NEXT.                              
           IF END-OF-FILE = 'N'                                         
               IF TRAN-PROC-TS (1:10) >= WS-START-DATE                  
                  AND TRAN-PROC-TS (1:10) <= WS-END-DATE                
                  NEXT SENTENCE                                         
               ELSE                                                     
                  GO TO 1000-PROCESS-TRANSACTIONS-EXIT.                 
                                                                        
           IF END-OF-FILE = 'N'                                         
               DISPLAY TRAN-RECORD                                      
               IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM                   
                  IF WS-FIRST-TIME = 'N'                                
                     PERFORM 1120-WRITE-ACCOUNT-TOTALS                  
                  ELSE                                                  
                     NEXT SENTENCE                                      
                  MOVE TRAN-CARD-NUM TO WS-CURR-CARD-NUM                
                  MOVE TRAN-CARD-NUM TO FD-XREF-CARD-NUM                
                  PERFORM 1500-A-LOOKUP-XREF                            
               ELSE                                                     
                  NEXT SENTENCE.                                        
                                                                        
           IF END-OF-FILE = 'N'                                         
               MOVE TRAN-TYPE-CD OF TRAN-RECORD TO FD-TRAN-TYPE         
               PERFORM 1500-B-LOOKUP-TRANTYPE                           
               MOVE TRAN-TYPE-CD OF TRAN-RECORD                         
                 TO FD-TRAN-TYPE-CD OF FD-TRAN-CAT-KEY                  
               MOVE TRAN-CAT-CD OF TRAN-RECORD                          
                 TO FD-TRAN-CAT-CD OF FD-TRAN-CAT-KEY                   
               PERFORM 1500-C-LOOKUP-TRANCATG                           
               PERFORM 1100-WRITE-TRANSACTION-REPORT                    
           ELSE                                                         
               DISPLAY 'TRAN-AMT ' TRAN-AMT                             
               DISPLAY 'WS-PAGE-TOTAL'  WS-PAGE-TOTAL                   
               ADD TRAN-AMT TO WS-PAGE-TOTAL                            
                               WS-ACCOUNT-TOTAL                         
               PERFORM 1110-WRITE-PAGE-TOTALS                           
               PERFORM 1110-WRITE-GRAND-TOTALS.                         
                                                                        
       1000-PROCESS-TRANSACTIONS-EXIT.                                  
           EXIT.
