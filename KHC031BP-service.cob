       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. KHC031BP.                                            
      *-----------------------------------------------------------------
      * AUTHOR.          SE:YAMAMOTO  PGM:KASUYA.                       
      * DATE-WRITTEN.     2008/01/11.                                   
      * DATE-COMPILED.    TODAY.                                        
      * REMARKS. 店別アウト用台帳作成Ｒ－１.                            
      *-----------------------------------------------------------------
      * テーブル Fetch Loop テーブル 更新                               
      * 修正履歴 管理ＮＯ(004668) 修正日付(20111020) 修正者(ZN)         
      * 修正履歴 管理ＮＯ(004722) 修正日付(20111117) 修正者(ZN)         
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           STDCALL IS DLL                                               
           ARGUMENT-NUMBER IS ARGNUM                                    
           ARGUMENT-VALUE  IS ARGVAL                                    
           .                                                            
       EXTERNAL-PROGRAM SECTION.                                        
           CALL-CONVENTION.                                             
           'KXU001SP' IS DLL                                            
           'KXU002SC' IS DLL                                            
           'KXU009SC' IS DLL                                            
           .                                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
      ******************************************************************
       WORKING-STORAGE   SECTION.                                       
                                                                        
      ** ログ情報エリア定義                                             
                                                                        
           COPY XAU0101    PREFIXING D-.                                
       01  KXU002SC               PIC  X(008) VALUE 'KXU002SC'.         
       01  KXU009SC               PIC  X(008) VALUE 'KXU009SC'.         
       01  WKPGMID                PIC  X(010) VALUE 'KHC031BP'.         
                                                                        
       01  ABND-CODE SYNC         PIC S9(004) COMP VALUE 16.            
       01  ARGCNT                 PIC  9(002) VALUE  0.                 
       01  ARGDATA                PIC  X(008) VALUE SPACE.              
       01  MAXARG                 PIC  9(001) VALUE  1.                 
                                                                        
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     
                                                                        
      ** ホスト変数定義                                                 
                                                                        
       01 W-抽出条件.                                                   
          03  W-当年抽出開始年月.                                       
            05  W-当年抽出開始年     PIC  X(4).                         
            05  W-当年抽出開始月     PIC  X(2).                         
          03  W-当年抽出終了年月.                                       
            05  W-当年抽出終了年     PIC  X(4).                         
            05  W-当年抽出終了月     PIC  X(2).                         
          03  W-前年抽出開始年月.                                       
            05  W-前年抽出開始年     PIC  X(4).                         
            05  W-前年抽出開始月     PIC  X(2).                         
          03  W-前年抽出終了年月.                                       
            05  W-前年抽出終了年     PIC  X(4).                         
            05  W-前年抽出終了月     PIC  X(2).                         
          03  W-４桁９タイプ       PIC  9(4).                           
       01 WK抽出条件.                                                   
          03  WK当年抽出開始年月     PIC  X(6).                         
          03  WK当年抽出終了年月     PIC  X(6).                         
          03  WK前年抽出開始年月     PIC  X(6).                         
          03  WK前年抽出終了年月     PIC  X(6).                         
          03  WK当年度             PIC  X(4).                           
          03  WK前年度             PIC  X(4).                           
          03  WK対象年月度         PIC  X(6).                           
          03  WK対象ＡＢ区分       PIC  X(1).                           
                                                                        
       01  W-COUNTER.                                                   
           03  SYS040-CNT         PIC  9(011)  VALUE 0.                 
           03  SYS045-CNT         PIC  9(011)  VALUE 0.                 
           03  INSERT-CNT         PIC  9(011)  VALUE 0.                 
           03  UPDATE-CNT         PIC  9(011)  VALUE 0.                 
           03  DELETE-CNT         PIC  9(011)  VALUE 0.                 
           03  ERROR-CNT          PIC  9(011)  VALUE 0.                 
           03  EXIST-CNT          PIC  9(011)  VALUE 0.                 
           03  NOT-EXIST-CNT      PIC  9(011)  VALUE 0.                 
                                                                        
           EXEC SQL END DECLARE SECTION END-EXEC.                       
                                                                        
           EXEC SQL INCLUDE SQLCA            END-EXEC.                  
           EXEC SQL INCLUDE XAU0201.CBL      END-EXEC.                  
           EXEC SQL INCLUDE XAT0201.CBL      END-EXEC.                  
           EXEC SQL INCLUDE XAU0901.CBL      END-EXEC.                  
           EXEC SQL INCLUDE HXU0102.CBL      END-EXEC.                  
                                                                        
      ******************************************************************
       PROCEDURE         DIVISION.                                      
      *-----------------------------------------------------------------
       主処理.                                                          
      *-----------------------------------------------------------------
                                                                        
      ** 初期処理                                                       
                                                                        
           MOVE  SPACE        TO D-ログ情報                             
           MOVE  WKPGMID      TO D-プログラムＩＤ                       
           MOVE  'STARTED   ' TO D-カウント見出し                       
           CALL  KXU002SC  USING  D-ログ情報                            
                                                                        
           MOVE 'HCM501'      TO ジョブコード                           
           MOVE  1            TO パラメータ番号                         
           MOVE  1            TO 行番号                                 
                                                                        
      ** メイン                                                         
                                                                        
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERROR END-EXEC.    
           PERFORM  ＤＢ接続情報取得処理.                               
           PERFORM  ＤＢ接続処理.                                       
           PERFORM  パラメータ取得処理.                                 
           PERFORM  パラメータログ出力処理.                             
           PERFORM  台帳削除処理.                                       
           PERFORM  台帳追加処理.                                       
           PERFORM  終了処理.                                           
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       ＤＢ接続情報取得処理.                                            
      *-----------------------------------------------------------------
           ACCEPT ARGCNT  FROM ARGNUM                                   
           IF ARGCNT > MAXARG                                           
             THEN                                                       
               MOVE '引数に誤りがあります'   TO D-メッセージ情報        
               CALL KXU002SC              USING D-ログ情報              
               MOVE ABND-CODE                TO RETURN-CODE             
               STOP RUN                                                 
             ELSE                                                       
               IF ARGCNT = 0                                            
                 THEN                                                   
                   MOVE SPACE                TO ARGDATA                 
                 ELSE                                                   
                   ACCEPT ARGDATA FROM ARGVAL                           
                     ON EXCEPTION                                       
                       MOVE '引数に誤りがあります' TO D-メッセージ情報  
                       CALL KXU002SC            USING D-ログ情報        
                       MOVE ABND-CODE              TO RETURN-CODE       
                       STOP RUN                                         
                     NOT ON EXCEPTION                                   
                       CONTINUE                                         
                   END-ACCEPT                                           
               END-IF                                                   
           END-IF                                                       
      *                                                                 
           CALL KXU009SC  USING  BY VALUE      ARGDATA                  
                                 BY REFERENCE  USERNAME                 
                                               PASSWD                   
                                               DB-NAME                  
           IF RETURN-CODE NOT = 0                                       
             THEN                                                       
               STOP RUN                                                 
             ELSE                                                       
               CONTINUE                                                 
           END-IF.                                                      
                                                                        
      *-----------------------------------------------------------------
       ＤＢ接続処理.                                                    
      *-----------------------------------------------------------------
           EXEC SQL                                                     
             CONNECT :USERNAME IDENTIFIED BY :PASSWD AT :DB-NAME        
           END-EXEC.                                                    
                                                                        
      *-----------------------------------------------------------------
       SQL-ERROR.                                                       
      *-----------------------------------------------------------------
           MOVE  'ＳＱＬエラー' TO D-メッセージ情報.                    
           CALL  KXU002SC  USING  D-ログ情報.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-メッセージ情報.           
           MOVE SQLERRMC TO D-メッセージ情報(25:100).                   
           CALL KXU002SC USING D-ログ情報.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       パラメータ取得処理.                                              
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND DO PERFORM SQL-ERRORP END-EXEC.  
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORP END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
             SELECT *                                                   
               INTO :パラメータファイル                                 
               FROM XAT_PARAM TP                                        
              WHERE TP.ジョブコード   = :ジョブコード                   
                AND TP.パラメータ番号 = :パラメータ番号                 
                AND TP.行番号         = :行番号                         
           END-EXEC.                                                    
           MOVE パラメータファイル  TO 年月指定パラメータREC(1:90)      
                                                                        
      *     IF 対象年月度(5:2) = '01' OR '02' OR '03'                    
      *       THEN                                                       
      *          MOVE 対象年月度(1:4)   TO W-４桁９タイプ                
      *          COMPUTE W-４桁９タイプ =  W-４桁９タイプ - 1            
      *          MOVE W-４桁９タイプ    TO W-当年抽出開始年              
      *       ELSE                                                       
      *          MOVE 対象年月度(1:4)   TO W-当年抽出開始年              
      *     END-IF                                                       
      *     MOVE '04'              TO W-当年抽出開始月                   
           MOVE 対象年月度(1:4)   TO W-当年抽出開始年              
           MOVE '01'              TO W-当年抽出開始月                   
                                                                        
           MOVE 対象年月度        TO W-当年抽出終了年月                 
                                                                        
           MOVE W-当年抽出開始年  TO W-４桁９タイプ                     
           COMPUTE W-４桁９タイプ  = W-４桁９タイプ - 1                 
           MOVE W-４桁９タイプ    TO W-前年抽出開始年                   
           MOVE W-当年抽出開始月  TO W-前年抽出開始月                   
                                                                        
           MOVE W-当年抽出終了年  TO W-４桁９タイプ                     
           COMPUTE W-４桁９タイプ  = W-４桁９タイプ - 1                 
           MOVE W-４桁９タイプ    TO W-前年抽出終了年                   
           MOVE W-当年抽出終了月  TO W-前年抽出終了月                   
                                                                        
           MOVE W-当年抽出開始年月  TO WK当年抽出開始年月               
           MOVE W-当年抽出終了年月  TO WK当年抽出終了年月               
           MOVE W-前年抽出開始年月  TO WK前年抽出開始年月               
           MOVE W-前年抽出終了年月  TO WK前年抽出終了年月               
           MOVE W-当年抽出開始年    TO WK当年度                         
           MOVE W-前年抽出開始年    TO WK前年度                         
           MOVE 対象年月度          TO WK対象年月度                     
           MOVE 対象ＡＢ区分        TO WK対象ＡＢ区分.                  
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORP.                                                      
      *-----------------------------------------------------------------
           MOVE  'ＳＱＬエラー パラメータ取得処理' TO D-メッセージ情報. 
           CALL  KXU002SC  USING  D-ログ情報.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-メッセージ情報.           
           MOVE SQLERRMC TO D-メッセージ情報(25:100).                   
           CALL KXU002SC USING D-ログ情報.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       パラメータログ出力処理.                                          
      *-----------------------------------------------------------------
           MOVE '販売システム  締時用パラメータ          '              
                                              TO D-メッセージ情報(1:40) 
           MOVE '対象年月度ＡＢ = @@@@@@*  '  TO D-メッセージ情報(41:27)
           MOVE  対象年月度                   TO D-メッセージ情報(58:6) 
           MOVE  対象ＡＢ区分                 TO D-メッセージ情報(64:1) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '請求処理年月   = @@@@@@   '  TO D-メッセージ情報(41:27)
           MOVE  請求処理年月                 TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '暦月処理年月   = @@@@@@   '  TO D-メッセージ情報(41:27)
           MOVE  暦月処理年月                 TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '適用開始年月日 = @@@@@@@@ '  TO D-メッセージ情報(41:27)
           MOVE  適用開始年月日               TO D-メッセージ情報(58:8) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '適用終了年月日 = @@@@@@@@ '  TO D-メッセージ情報(41:27)
           MOVE  適用終了年月日               TO D-メッセージ情報(58:8) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '当年抽出開始年月=@@@@MM   '  TO D-メッセージ情報(41:27)
           MOVE  WK当年抽出開始年月           TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '当年抽出終了年月=@@@@MM   '  TO D-メッセージ情報(41:27)
           MOVE  WK当年抽出終了年月           TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '前年抽出開始年月=@@@@MM   '  TO D-メッセージ情報(41:27)
           MOVE  WK前年抽出開始年月           TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報                             
                                                                        
           MOVE '前年抽出終了年月=@@@@MM   '  TO D-メッセージ情報(41:27)
           MOVE  WK前年抽出終了年月           TO D-メッセージ情報(58:6) 
           CALL KXU002SC  USING  D-ログ情報.                            
                                                                        
      *-----------------------------------------------------------------
       台帳削除処理.                                                    
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORD END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            DELETE FROM  HCT_OUT_MISE_DFILE_NEW TD                          
              WHERE TD.暦月処理年月 <= :WK対象年月度                    
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  DELETE-CNT.                            
                                                                        
           MOVE '台帳削除処理　完了' TO D-メッセージ情報.               
           CALL KXU002SC   USING  D-ログ情報.                           
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORD.                                                      
      *-----------------------------------------------------------------
           MOVE  'ＳＱＬエラー 台帳削除処理' TO D-メッセージ情報.       
           CALL  KXU002SC  USING  D-ログ情報.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-メッセージ情報.           
           MOVE SQLERRMC TO D-メッセージ情報(25:100).                   
           CALL KXU002SC USING D-ログ情報.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       台帳追加処理.                                                    
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORI END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            INSERT  INTO  HCT_OUT_MISE_DFILE_NEW                            
            SELECT                                                      
               :WK当年度                                                
              ,:WK対象年月度                                            
              ,T.得意先コード枝親                                       
              ,T.ブランドコード                                         
              , CASE T.内部ＢＫ0                                        
                WHEN 'ZD0' THEN 'XA0'                                   
                WHEN 'ZF0' THEN 'XB0'                                   
                WHEN 'ZE0' THEN 'XC0'                                   
                WHEN 'ZG0' THEN 'XD0'                                   
                ELSE T.内部ＢＫ0                                        
                END                                                     
              ,T.取引状態                                               
              ,T.取引年月日                                             
              ,T.解約年月日                                             
              ,T.取引状態総合                                           
              ,T.取引年月日総合                                         
              ,T.解約年月日総合                                         
              ,T.チャネル区分                                           
              ,T.新チャネル区分                                         
              ,T.ＳＲ区分                                               
              ,T.クラスコード                                           
              ,T.業態２区分                                             
              ,T.領収書区分                                             
              ,T.割戻相殺                                               
              ,T.入金方法                                               
              ,T.支払条件定義キー                                       
              ,T.自動引落対象                                           
              ,T.請求書フォーマット                                     
              ,T.前年月度新規区分                                       
              ,T.前年累計新規区分                                       
              ,T.当年月度新規区分                                       
              ,T.当年累計新規区分                                       
              ,T.前年月度新規区分総合                                   
              ,T.前年累計新規区分総合                                   
              ,T.当年月度新規区分総合                                   
              ,T.当年累計新規区分総合                                   
              ,T.前年月度解約区分                                       
              ,T.前年累計解約区分                                       
              ,T.当年月度解約区分                                       
              ,T.当年累計解約区分                                       
              ,T.前年月度解約区分総合                                   
              ,T.前年累計解約区分総合                                   
              ,T.当年月度解約区分総合                                   
              ,T.当年累計解約区分総合                                   
              ,NVL(S.前年送品01,0) AS 前年送品01                        
              ,NVL(S.前年送品02,0) AS 前年送品02                        
              ,NVL(S.前年送品03,0) AS 前年送品03                        
              ,NVL(S.前年送品04,0) AS 前年送品04                        
              ,NVL(S.前年送品05,0) AS 前年送品05                        
              ,NVL(S.前年送品06,0) AS 前年送品06                        
              ,NVL(S.前年送品07,0) AS 前年送品07                        
              ,NVL(S.前年送品08,0) AS 前年送品08                        
              ,NVL(S.前年送品09,0) AS 前年送品09                        
              ,NVL(S.前年送品10,0) AS 前年送品10                        
              ,NVL(S.前年送品11,0) AS 前年送品11                        
              ,NVL(S.前年送品12,0) AS 前年送品12                        
              ,NVL(S.前年送品上期,0) AS 前年送品上期                    
              ,NVL(S.前年送品下期,0) AS 前年送品下期                    
              ,NVL(S.前年送品累計,0) AS 前年送品累計                    
              ,NVL(S.前年純送01,0) AS 前年純送01                        
              ,NVL(S.前年純送02,0) AS 前年純送02                        
              ,NVL(S.前年純送03,0) AS 前年純送03                        
              ,NVL(S.前年純送04,0) AS 前年純送04                        
              ,NVL(S.前年純送05,0) AS 前年純送05                        
              ,NVL(S.前年純送06,0) AS 前年純送06                        
              ,NVL(S.前年純送07,0) AS 前年純送07                        
              ,NVL(S.前年純送08,0) AS 前年純送08                        
              ,NVL(S.前年純送09,0) AS 前年純送09                        
              ,NVL(S.前年純送10,0) AS 前年純送10                        
              ,NVL(S.前年純送11,0) AS 前年純送11                        
              ,NVL(S.前年純送12,0) AS 前年純送12                        
              ,NVL(S.前年純送上期,0) AS 前年純送上期                    
              ,NVL(S.前年純送下期,0) AS 前年純送下期                    
              ,NVL(S.前年純送累計,0) AS 前年純送累計                    
              ,NVL(S.前年Ａ返01,0) AS 前年Ａ返01                        
              ,NVL(S.前年Ａ返02,0) AS 前年Ａ返02                        
              ,NVL(S.前年Ａ返03,0) AS 前年Ａ返03                        
              ,NVL(S.前年Ａ返04,0) AS 前年Ａ返04                        
              ,NVL(S.前年Ａ返05,0) AS 前年Ａ返05                        
              ,NVL(S.前年Ａ返06,0) AS 前年Ａ返06                        
              ,NVL(S.前年Ａ返07,0) AS 前年Ａ返07                        
              ,NVL(S.前年Ａ返08,0) AS 前年Ａ返08                        
              ,NVL(S.前年Ａ返09,0) AS 前年Ａ返09                        
              ,NVL(S.前年Ａ返10,0) AS 前年Ａ返10                        
              ,NVL(S.前年Ａ返11,0) AS 前年Ａ返11                        
              ,NVL(S.前年Ａ返12,0) AS 前年Ａ返12                        
              ,NVL(S.前年Ａ返上期,0) AS 前年Ａ返上期                    
              ,NVL(S.前年Ａ返下期,0) AS 前年Ａ返下期                    
              ,NVL(S.前年Ａ返累計,0) AS 前年Ａ返累計                    
              ,NVL(S.前年Ｂ返01,0) AS 前年Ｂ返01                        
              ,NVL(S.前年Ｂ返02,0) AS 前年Ｂ返02                        
              ,NVL(S.前年Ｂ返03,0) AS 前年Ｂ返03                        
              ,NVL(S.前年Ｂ返04,0) AS 前年Ｂ返04                        
              ,NVL(S.前年Ｂ返05,0) AS 前年Ｂ返05                        
              ,NVL(S.前年Ｂ返06,0) AS 前年Ｂ返06                        
              ,NVL(S.前年Ｂ返07,0) AS 前年Ｂ返07                        
              ,NVL(S.前年Ｂ返08,0) AS 前年Ｂ返08                        
              ,NVL(S.前年Ｂ返09,0) AS 前年Ｂ返09                        
              ,NVL(S.前年Ｂ返10,0) AS 前年Ｂ返10                        
              ,NVL(S.前年Ｂ返11,0) AS 前年Ｂ返11                        
              ,NVL(S.前年Ｂ返12,0) AS 前年Ｂ返12                        
              ,NVL(S.前年Ｂ返上期,0) AS 前年Ｂ返上期                    
              ,NVL(S.前年Ｂ返下期,0) AS 前年Ｂ返下期                    
              ,NVL(S.前年Ｂ返累計,0) AS 前年Ｂ返累計                    
              ,NVL(S.前年解返01,0) AS 前年解返01                        
              ,NVL(S.前年解返02,0) AS 前年解返02                        
              ,NVL(S.前年解返03,0) AS 前年解返03                        
              ,NVL(S.前年解返04,0) AS 前年解返04                        
              ,NVL(S.前年解返05,0) AS 前年解返05                        
              ,NVL(S.前年解返06,0) AS 前年解返06                        
              ,NVL(S.前年解返07,0) AS 前年解返07                        
              ,NVL(S.前年解返08,0) AS 前年解返08                        
              ,NVL(S.前年解返09,0) AS 前年解返09                        
              ,NVL(S.前年解返10,0) AS 前年解返10                        
              ,NVL(S.前年解返11,0) AS 前年解返11                        
              ,NVL(S.前年解返12,0) AS 前年解返12                        
              ,NVL(S.前年解返上期,0) AS 前年解返上期                    
              ,NVL(S.前年解返下期,0) AS 前年解返下期                    
              ,NVL(S.前年解返累計,0) AS 前年解返累計                    
              ,NVL(S.前年純入金01,0) AS 前年純入金01                    
              ,NVL(S.前年純入金02,0) AS 前年純入金02                    
              ,NVL(S.前年純入金03,0) AS 前年純入金03                    
              ,NVL(S.前年純入金04,0) AS 前年純入金04                    
              ,NVL(S.前年純入金05,0) AS 前年純入金05                    
              ,NVL(S.前年純入金06,0) AS 前年純入金06                    
              ,NVL(S.前年純入金07,0) AS 前年純入金07                    
              ,NVL(S.前年純入金08,0) AS 前年純入金08                    
              ,NVL(S.前年純入金09,0) AS 前年純入金09                    
              ,NVL(S.前年純入金10,0) AS 前年純入金10                    
              ,NVL(S.前年純入金11,0) AS 前年純入金11                    
              ,NVL(S.前年純入金12,0) AS 前年純入金12                    
              ,NVL(S.前年純入金上期,0) AS 前年純入金上期                
              ,NVL(S.前年純入金下期,0) AS 前年純入金下期                
              ,NVL(S.前年純入金累計,0) AS 前年純入金累計                
              ,NVL(S.前年責任額01,0) AS 前年責任額01                    
              ,NVL(S.前年責任額02,0) AS 前年責任額02                    
              ,NVL(S.前年責任額03,0) AS 前年責任額03                    
              ,NVL(S.前年責任額04,0) AS 前年責任額04                    
              ,NVL(S.前年責任額05,0) AS 前年責任額05                    
              ,NVL(S.前年責任額06,0) AS 前年責任額06                    
              ,NVL(S.前年責任額07,0) AS 前年責任額07                    
              ,NVL(S.前年責任額08,0) AS 前年責任額08                    
              ,NVL(S.前年責任額09,0) AS 前年責任額09                    
              ,NVL(S.前年責任額10,0) AS 前年責任額10                    
              ,NVL(S.前年責任額11,0) AS 前年責任額11                    
              ,NVL(S.前年責任額12,0) AS 前年責任額12                    
              ,NVL(S.前年責任額上期,0) AS 前年責任額上期                
              ,NVL(S.前年責任額下期,0) AS 前年責任額下期                
              ,NVL(S.前年責任額累計,0) AS 前年責任額累計                
              ,NVL(S.当年送品01,0) AS 当年送品01                        
              ,NVL(S.当年送品02,0) AS 当年送品02                        
              ,NVL(S.当年送品03,0) AS 当年送品03                        
              ,NVL(S.当年送品04,0) AS 当年送品04                        
              ,NVL(S.当年送品05,0) AS 当年送品05                        
              ,NVL(S.当年送品06,0) AS 当年送品06                        
              ,NVL(S.当年送品07,0) AS 当年送品07                        
              ,NVL(S.当年送品08,0) AS 当年送品08                        
              ,NVL(S.当年送品09,0) AS 当年送品09                        
              ,NVL(S.当年送品10,0) AS 当年送品10                        
              ,NVL(S.当年送品11,0) AS 当年送品11                        
              ,NVL(S.当年送品12,0) AS 当年送品12                        
              ,NVL(S.当年送品上期,0) AS 当年送品上期                    
              ,NVL(S.当年送品下期,0) AS 当年送品下期                    
              ,NVL(S.当年送品累計,0) AS 当年送品累計                    
              ,NVL(S.当年純送01,0) AS 当年純送01                        
              ,NVL(S.当年純送02,0) AS 当年純送02                        
              ,NVL(S.当年純送03,0) AS 当年純送03                        
              ,NVL(S.当年純送04,0) AS 当年純送04                        
              ,NVL(S.当年純送05,0) AS 当年純送05                        
              ,NVL(S.当年純送06,0) AS 当年純送06                        
              ,NVL(S.当年純送07,0) AS 当年純送07                        
              ,NVL(S.当年純送08,0) AS 当年純送08                        
              ,NVL(S.当年純送09,0) AS 当年純送09                        
              ,NVL(S.当年純送10,0) AS 当年純送10                        
              ,NVL(S.当年純送11,0) AS 当年純送11                        
              ,NVL(S.当年純送12,0) AS 当年純送12                        
              ,NVL(S.当年純送上期,0) AS 当年純送上期                    
              ,NVL(S.当年純送下期,0) AS 当年純送下期                    
              ,NVL(S.当年純送累計,0) AS 当年純送累計                    
              ,NVL(S.当年Ａ返01,0) AS 当年Ａ返01                        
              ,NVL(S.当年Ａ返02,0) AS 当年Ａ返02                        
              ,NVL(S.当年Ａ返03,0) AS 当年Ａ返03                        
              ,NVL(S.当年Ａ返04,0) AS 当年Ａ返04                        
              ,NVL(S.当年Ａ返05,0) AS 当年Ａ返05                        
              ,NVL(S.当年Ａ返06,0) AS 当年Ａ返06                        
              ,NVL(S.当年Ａ返07,0) AS 当年Ａ返07                        
              ,NVL(S.当年Ａ返08,0) AS 当年Ａ返08                        
              ,NVL(S.当年Ａ返09,0) AS 当年Ａ返09                        
              ,NVL(S.当年Ａ返10,0) AS 当年Ａ返10                        
              ,NVL(S.当年Ａ返11,0) AS 当年Ａ返11                        
              ,NVL(S.当年Ａ返12,0) AS 当年Ａ返12                        
              ,NVL(S.当年Ａ返上期,0) AS 当年Ａ返上期                    
              ,NVL(S.当年Ａ返下期,0) AS 当年Ａ返下期                    
              ,NVL(S.当年Ａ返累計,0) AS 当年Ａ返累計                    
              ,NVL(S.当年Ｂ返01,0) AS 当年Ｂ返01                        
              ,NVL(S.当年Ｂ返02,0) AS 当年Ｂ返02                        
              ,NVL(S.当年Ｂ返03,0) AS 当年Ｂ返03                        
              ,NVL(S.当年Ｂ返04,0) AS 当年Ｂ返04                        
              ,NVL(S.当年Ｂ返05,0) AS 当年Ｂ返05                        
              ,NVL(S.当年Ｂ返06,0) AS 当年Ｂ返06                        
              ,NVL(S.当年Ｂ返07,0) AS 当年Ｂ返07                        
              ,NVL(S.当年Ｂ返08,0) AS 当年Ｂ返08                        
              ,NVL(S.当年Ｂ返09,0) AS 当年Ｂ返09                        
              ,NVL(S.当年Ｂ返10,0) AS 当年Ｂ返10                        
              ,NVL(S.当年Ｂ返11,0) AS 当年Ｂ返11                        
              ,NVL(S.当年Ｂ返12,0) AS 当年Ｂ返12                        
              ,NVL(S.当年Ｂ返上期,0) AS 当年Ｂ返上期                    
              ,NVL(S.当年Ｂ返下期,0) AS 当年Ｂ返下期                    
              ,NVL(S.当年Ｂ返累計,0) AS 当年Ｂ返累計                    
              ,NVL(S.当年解返01,0) AS 当年解返01                        
              ,NVL(S.当年解返02,0) AS 当年解返02                        
              ,NVL(S.当年解返03,0) AS 当年解返03                        
              ,NVL(S.当年解返04,0) AS 当年解返04                        
              ,NVL(S.当年解返05,0) AS 当年解返05                        
              ,NVL(S.当年解返06,0) AS 当年解返06                        
              ,NVL(S.当年解返07,0) AS 当年解返07                        
              ,NVL(S.当年解返08,0) AS 当年解返08                        
              ,NVL(S.当年解返09,0) AS 当年解返09                        
              ,NVL(S.当年解返10,0) AS 当年解返10                        
              ,NVL(S.当年解返11,0) AS 当年解返11                        
              ,NVL(S.当年解返12,0) AS 当年解返12                        
              ,NVL(S.当年解返上期,0) AS 当年解返上期                    
              ,NVL(S.当年解返下期,0) AS 当年解返下期                    
              ,NVL(S.当年解返累計,0) AS 当年解返累計                    
              ,NVL(S.当年純入金01,0) AS 当年純入金01                    
              ,NVL(S.当年純入金02,0) AS 当年純入金02                    
              ,NVL(S.当年純入金03,0) AS 当年純入金03                    
              ,NVL(S.当年純入金04,0) AS 当年純入金04                    
              ,NVL(S.当年純入金05,0) AS 当年純入金05                    
              ,NVL(S.当年純入金06,0) AS 当年純入金06                    
              ,NVL(S.当年純入金07,0) AS 当年純入金07                    
              ,NVL(S.当年純入金08,0) AS 当年純入金08                    
              ,NVL(S.当年純入金09,0) AS 当年純入金09                    
              ,NVL(S.当年純入金10,0) AS 当年純入金10                   
              ,NVL(S.当年純入金11,0) AS 当年純入金11                    
              ,NVL(S.当年純入金12,0) AS 当年純入金12                    
              ,NVL(S.当年純入金上期,0) AS 当年純入金上期                
              ,NVL(S.当年純入金下期,0) AS 当年純入金下期                
              ,NVL(S.当年純入金累計,0) AS 当年純入金累計                
              ,NVL(S.当年責任額01,0) AS 当年責任額01                    
              ,NVL(S.当年責任額02,0) AS 当年責任額02                    
              ,NVL(S.当年責任額03,0) AS 当年責任額03                    
              ,NVL(S.当年責任額04,0) AS 当年責任額04                    
              ,NVL(S.当年責任額05,0) AS 当年責任額05                    
              ,NVL(S.当年責任額06,0) AS 当年責任額06                    
              ,NVL(S.当年責任額07,0) AS 当年責任額07                    
              ,NVL(S.当年責任額08,0) AS 当年責任額08                    
              ,NVL(S.当年責任額09,0) AS 当年責任額09                    
              ,NVL(S.当年責任額10,0) AS 当年責任額10                    
              ,NVL(S.当年責任額11,0) AS 当年責任額11                    
              ,NVL(S.当年責任額12,0) AS 当年責任額12                    
              ,NVL(S.当年責任額上期,0) AS 当年責任額上期                
              ,NVL(S.当年責任額下期,0) AS 当年責任額下期                
              ,NVL(S.当年責任額累計,0) AS 当年責任額累計                
             FROM                                                       
              (                                                         
              SELECT                                                    
                 T1.得意先コード枝親                                    
                ,T1.内部ＢＫ||'0' AS 内部ＢＫ0                          
                ,T2.ブランドコード                                      
                ,T2.取引状態                                            
                ,T2.取引年月日                                          
                ,T2.解約年月日                                          
                ,T1.取引状態総合                                        
                ,T1.取引年月日総合                                      
                ,T1.解約年月日総合                                      
                ,T1.チャネル区分                                        
                ,T1.新チャネル区分                                      
                ,T1.ＳＲ区分                                            
                ,T1.クラスコード                                        
                ,T1.業態２区分                                          
                ,T1.領収書区分                                          
                ,T1.割戻相殺                                            
                ,T3.入金方法                                            
                ,T3.支払条件定義キー                                    
                ,T3.自動引落対象                                        
                ,T3.請求書フォーマット                                  
                                                                        
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) =                  
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) BETWEEN            
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) =                  
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) BETWEEN            
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計新規区分         
                                                                        
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) =              
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) BETWEEN        
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) =              
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) BETWEEN        
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計新規区分総合     
                                                                        
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) =                  
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) BETWEEN            
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) =                  
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) BETWEEN            
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計解約区分         
                                                                        
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) =              
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) BETWEEN        
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) =              
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) BETWEEN        
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計解約区分総合     
               FROM                                                     
                  HAT_TOKUI_KIHON  T1                                   
                , HAT_TOKUI_BRAND  T2                                   
                , HAT_TOKUI_SYOSAI T3                                   
               WHERE                                                    
                     T1.対象年月 = :WK対象年月度                        
                 AND T1.対象ＡＢ区分 = :WK対象ＡＢ区分                  
                 AND T1.得意先コード = T1.得意先コード枝親              
                 AND T2.得意先コード = T2.得意先コード枝親              
                 AND T3.得意先コード = T3.得意先コード枝親           
                 AND T2.ブランドコード NOT IN ('02', '20', '21', '23',
                                               '13', '66','30',
                                               '80', '81', '82')        
      *           AND T2.取引状態 IN ('0', '1', '2', '3')                 
                 AND T1.得意先コード = T2.得意先コード                  
                 AND T1.対象年月 = T2.対象年月                          
                 AND T1.対象ＡＢ区分 = T2.対象ＡＢ区分                  
                 AND T1.得意先コード = T3.得意先コード                  
                 AND T1.対象年月 = T3.対象年月                          
                 AND T1.対象ＡＢ区分 = T3.対象ＡＢ区分                  
                 AND SUBSTR(T1.支店部署コード,1,1) <> 'X'
              ) T                                                       
             ,(                                                         
              SELECT                                                    
                 S1.得意先コード枝親                                    
                ,NVL(D1.ブランド変換コード,S1.ブランドコード)           
                  AS ブランドコード                                     
       /* 前年送品 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品01                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品02                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品03                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品04                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品05                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品06                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品07                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品08                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品09                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                   THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END) 
                  AS 前年送品10                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品11                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品12                                         
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品上期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品下期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品累計                                       
                                                                        
       /* 前年純送 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送累計        
                                                                        
       /* 前年Ａ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返累計        
                                                                        
       /* 前年Ｂ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返累計        
                                                                        
       /* 前年解返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返累計      
                                                                        
       /* 前年純入金 */                                                 
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金11       
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金累計      
                                                                        
       /* 前年責任額 */                                                 
                ,'0' AS 前年責任額01                                    
                ,'0' AS 前年責任額02                                    
                ,'0' AS 前年責任額03                                    
                ,'0' AS 前年責任額04                                    
                ,'0' AS 前年責任額05                                    
                ,'0' AS 前年責任額06                                    
                ,'0' AS 前年責任額07                                    
                ,'0' AS 前年責任額08                                    
                ,'0' AS 前年責任額09                                    
                ,'0' AS 前年責任額10                                    
                ,'0' AS 前年責任額11                                    
                ,'0' AS 前年責任額12                                    
                ,'0' AS 前年責任額上期                                  
                ,'0' AS 前年責任額下期                                  
                ,'0' AS 前年責任額累計                                  
                                                                        
       /* 当年送品 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品01                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品02                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品03                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品04                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品05                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品06                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品07                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品08                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品09                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品10                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品11                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品12                                         
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品上期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品下期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品累計                                       
                                                                        
       /* 当年純送 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送累計        
                                                                        
       /* 当年Ａ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返累計        
                                                                        
       /* 当年Ｂ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返累計        
                                                                        
       /* 当年解返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返累計      
                                                                        
       /* 当年純入金 */                                                 
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金累計      
                                                                        
       /* 当年責任額 */                                                 
                ,'0' AS 当年責任額01                                    
                ,'0' AS 当年責任額02                                    
                ,'0' AS 当年責任額03                                    
                ,'0' AS 当年責任額04                                    
                ,'0' AS 当年責任額05                                    
                ,'0' AS 当年責任額06                                    
                ,'0' AS 当年責任額07                                    
                ,'0' AS 当年責任額08                                    
                ,'0' AS 当年責任額09                                    
                ,'0' AS 当年責任額10                                    
                ,'0' AS 当年責任額11                                    
                ,'0' AS 当年責任額12                                    
                ,'0' AS 当年責任額上期                                  
                ,'0' AS 当年責任額下期                                  
                ,'0' AS 当年責任額累計                                  
                                                                        
               FROM                                                     
                  HCT_SALES_PERFORMANCE  S1                             
                ,(SELECT '02' AS ブランドコード                         
                        ,'01' AS ブランド変換コード FROM dual           
                  UNION ALL                                             
                  SELECT '20','19' FROM dual                            
                  UNION ALL                                             
                  SELECT '21','10' FROM dual                            
       /* 2011.10.20 ADD ﾎﾟｰﾙ追加 */   
                  UNION ALL                                             
                  SELECT '66','65' FROM dual                            
       /* 2011.11.17 ADD CVS50追加 */   
                  UNION ALL                                             
                  SELECT '30','12' FROM dual                            
                  UNION ALL                                             
                  SELECT '23','22' FROM dual                            
                 ) D1                                                   
               WHERE                                                    
                     S1.ブランドコード = D1.ブランドコード (+)          
                 AND S1.ブランドコード NOT IN ('90', '91', '92')        
                 AND (  (   S1.暦月処理年月 >= :WK前年抽出開始年月      
                        AND S1.暦月処理年月 <= :WK前年抽出終了年月)     
                     OR (   S1.暦月処理年月 >= :WK当年抽出開始年月      
                        AND S1.暦月処理年月 <= :WK当年抽出終了年月) )   
               GROUP BY                                                 
                     S1.得意先コード枝親                                
                    ,NVL(D1.ブランド変換コード,S1.ブランドコード)       
              ) S                                                       
                                                                        
             WHERE                                                      
                   T.得意先コード枝親 = S.得意先コード枝親 (+)          
               AND T.ブランドコード = S.ブランドコード (+)              
                                                                        
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  INSERT-CNT.                            
                                                                        
           MOVE 'INSERT   ='   TO D-カウント見出し                      
           MOVE INSERT-CNT     TO D-カウント                            
           CALL KXU002SC    USING D-ログ情報                            

           MOVE '台帳追加処理１完了' TO D-メッセージ情報.               
           CALL KXU002SC   USING  D-ログ情報.                           
                                                                        
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORI END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            INSERT  INTO  HCT_OUT_MISE_DFILE_NEW                        
            SELECT                                                      
               :WK当年度                                                
              ,:WK対象年月度                                            
              ,T.得意先コード枝親                                       
              ,T.ブランドコード                                         
              ,T.内部ＢＫ0                                              
              ,T.取引状態                                               
              ,T.取引年月日                                             
              ,T.解約年月日                                             
              ,T.取引状態総合                                           
              ,T.取引年月日総合                                         
              ,T.解約年月日総合                                         
              ,T.チャネル区分                                           
              ,T.新チャネル区分                                         
              ,T.ＳＲ区分                                               
              ,T.クラスコード                                           
              ,T.業態２区分                                             
              ,T.領収書区分                                             
              ,T.割戻相殺                                               
              ,T.入金方法                                               
              ,T.支払条件定義キー                                       
              ,T.自動引落対象                                           
              ,T.請求書フォーマット                                     
              ,T.前年月度新規区分                                       
              ,T.前年累計新規区分                                       
              ,T.当年月度新規区分                                       
              ,T.当年累計新規区分                                       
              ,T.前年月度新規区分総合                                   
              ,T.前年累計新規区分総合                                   
              ,T.当年月度新規区分総合                                   
              ,T.当年累計新規区分総合                                   
              ,T.前年月度解約区分                                       
              ,T.前年累計解約区分                                       
              ,T.当年月度解約区分                                       
              ,T.当年累計解約区分                                       
              ,T.前年月度解約区分総合                                   
              ,T.前年累計解約区分総合                                   
              ,T.当年月度解約区分総合                                   
              ,T.当年累計解約区分総合                                   
              ,NVL(S.前年送品01,0) AS 前年送品01                        
              ,NVL(S.前年送品02,0) AS 前年送品02                        
              ,NVL(S.前年送品03,0) AS 前年送品03                        
              ,NVL(S.前年送品04,0) AS 前年送品04                        
              ,NVL(S.前年送品05,0) AS 前年送品05                        
              ,NVL(S.前年送品06,0) AS 前年送品06                        
              ,NVL(S.前年送品07,0) AS 前年送品07                        
              ,NVL(S.前年送品08,0) AS 前年送品08                        
              ,NVL(S.前年送品09,0) AS 前年送品09                        
              ,NVL(S.前年送品10,0) AS 前年送品10                        
              ,NVL(S.前年送品11,0) AS 前年送品11                        
              ,NVL(S.前年送品12,0) AS 前年送品12                        
              ,NVL(S.前年送品上期,0) AS 前年送品上期                    
              ,NVL(S.前年送品下期,0) AS 前年送品下期                    
              ,NVL(S.前年送品累計,0) AS 前年送品累計                    
              ,NVL(S.前年純送01,0) AS 前年純送01                        
              ,NVL(S.前年純送02,0) AS 前年純送02                        
              ,NVL(S.前年純送03,0) AS 前年純送03                        
              ,NVL(S.前年純送04,0) AS 前年純送04                        
              ,NVL(S.前年純送05,0) AS 前年純送05                        
              ,NVL(S.前年純送06,0) AS 前年純送06                        
              ,NVL(S.前年純送07,0) AS 前年純送07                        
              ,NVL(S.前年純送08,0) AS 前年純送08                        
              ,NVL(S.前年純送09,0) AS 前年純送09                        
              ,NVL(S.前年純送10,0) AS 前年純送10                        
              ,NVL(S.前年純送11,0) AS 前年純送11                        
              ,NVL(S.前年純送12,0) AS 前年純送12                        
              ,NVL(S.前年純送上期,0) AS 前年純送上期                    
              ,NVL(S.前年純送下期,0) AS 前年純送下期                    
              ,NVL(S.前年純送累計,0) AS 前年純送累計                    
              ,NVL(S.前年Ａ返01,0) AS 前年Ａ返01                        
              ,NVL(S.前年Ａ返02,0) AS 前年Ａ返02                        
              ,NVL(S.前年Ａ返03,0) AS 前年Ａ返03                        
              ,NVL(S.前年Ａ返04,0) AS 前年Ａ返04                        
              ,NVL(S.前年Ａ返05,0) AS 前年Ａ返05                        
              ,NVL(S.前年Ａ返06,0) AS 前年Ａ返06                        
              ,NVL(S.前年Ａ返07,0) AS 前年Ａ返07                        
              ,NVL(S.前年Ａ返08,0) AS 前年Ａ返08                        
              ,NVL(S.前年Ａ返09,0) AS 前年Ａ返09                        
              ,NVL(S.前年Ａ返10,0) AS 前年Ａ返10                        
              ,NVL(S.前年Ａ返11,0) AS 前年Ａ返11                        
              ,NVL(S.前年Ａ返12,0) AS 前年Ａ返12                        
              ,NVL(S.前年Ａ返上期,0) AS 前年Ａ返上期                    
              ,NVL(S.前年Ａ返下期,0) AS 前年Ａ返下期                    
              ,NVL(S.前年Ａ返累計,0) AS 前年Ａ返累計                    
              ,NVL(S.前年Ｂ返01,0) AS 前年Ｂ返01                        
              ,NVL(S.前年Ｂ返02,0) AS 前年Ｂ返02                        
              ,NVL(S.前年Ｂ返03,0) AS 前年Ｂ返03                        
              ,NVL(S.前年Ｂ返04,0) AS 前年Ｂ返04                        
              ,NVL(S.前年Ｂ返05,0) AS 前年Ｂ返05                        
              ,NVL(S.前年Ｂ返06,0) AS 前年Ｂ返06                        
              ,NVL(S.前年Ｂ返07,0) AS 前年Ｂ返07                        
              ,NVL(S.前年Ｂ返08,0) AS 前年Ｂ返08                        
              ,NVL(S.前年Ｂ返09,0) AS 前年Ｂ返09                        
              ,NVL(S.前年Ｂ返10,0) AS 前年Ｂ返10                        
              ,NVL(S.前年Ｂ返11,0) AS 前年Ｂ返11                        
              ,NVL(S.前年Ｂ返12,0) AS 前年Ｂ返12                        
              ,NVL(S.前年Ｂ返上期,0) AS 前年Ｂ返上期                    
              ,NVL(S.前年Ｂ返下期,0) AS 前年Ｂ返下期                    
              ,NVL(S.前年Ｂ返累計,0) AS 前年Ｂ返累計                    
              ,NVL(S.前年解返01,0) AS 前年解返01                        
              ,NVL(S.前年解返02,0) AS 前年解返02                        
              ,NVL(S.前年解返03,0) AS 前年解返03                        
              ,NVL(S.前年解返04,0) AS 前年解返04                        
              ,NVL(S.前年解返05,0) AS 前年解返05                        
              ,NVL(S.前年解返06,0) AS 前年解返06                        
              ,NVL(S.前年解返07,0) AS 前年解返07                        
              ,NVL(S.前年解返08,0) AS 前年解返08                        
              ,NVL(S.前年解返09,0) AS 前年解返09                        
              ,NVL(S.前年解返10,0) AS 前年解返10                        
              ,NVL(S.前年解返11,0) AS 前年解返11                        
              ,NVL(S.前年解返12,0) AS 前年解返12                        
              ,NVL(S.前年解返上期,0) AS 前年解返上期                    
              ,NVL(S.前年解返下期,0) AS 前年解返下期                    
              ,NVL(S.前年解返累計,0) AS 前年解返累計                    
              ,NVL(S.前年純入金01,0) AS 前年純入金01                    
              ,NVL(S.前年純入金02,0) AS 前年純入金02                    
              ,NVL(S.前年純入金03,0) AS 前年純入金03                    
              ,NVL(S.前年純入金04,0) AS 前年純入金04                    
              ,NVL(S.前年純入金05,0) AS 前年純入金05                    
              ,NVL(S.前年純入金06,0) AS 前年純入金06                    
              ,NVL(S.前年純入金07,0) AS 前年純入金07                    
              ,NVL(S.前年純入金08,0) AS 前年純入金08                    
              ,NVL(S.前年純入金09,0) AS 前年純入金09                    
              ,NVL(S.前年純入金10,0) AS 前年純入金10                    
              ,NVL(S.前年純入金11,0) AS 前年純入金11                    
              ,NVL(S.前年純入金12,0) AS 前年純入金12                    
              ,NVL(S.前年純入金上期,0) AS 前年純入金上期                
              ,NVL(S.前年純入金下期,0) AS 前年純入金下期                
              ,NVL(S.前年純入金累計,0) AS 前年純入金累計                
              ,NVL(S.前年責任額01,0) AS 前年責任額01                    
              ,NVL(S.前年責任額02,0) AS 前年責任額02                    
              ,NVL(S.前年責任額03,0) AS 前年責任額03                    
              ,NVL(S.前年責任額04,0) AS 前年責任額04                    
              ,NVL(S.前年責任額05,0) AS 前年責任額05                    
              ,NVL(S.前年責任額06,0) AS 前年責任額06                    
              ,NVL(S.前年責任額07,0) AS 前年責任額07                    
              ,NVL(S.前年責任額08,0) AS 前年責任額08                    
              ,NVL(S.前年責任額09,0) AS 前年責任額09                    
              ,NVL(S.前年責任額10,0) AS 前年責任額10                    
              ,NVL(S.前年責任額11,0) AS 前年責任額11                    
              ,NVL(S.前年責任額12,0) AS 前年責任額12                    
              ,NVL(S.前年責任額上期,0) AS 前年責任額上期                
              ,NVL(S.前年責任額下期,0) AS 前年責任額下期                
              ,NVL(S.前年責任額累計,0) AS 前年責任額累計                
              ,NVL(S.当年送品01,0) AS 当年送品01                        
              ,NVL(S.当年送品02,0) AS 当年送品02                        
              ,NVL(S.当年送品03,0) AS 当年送品03                        
              ,NVL(S.当年送品04,0) AS 当年送品04                        
              ,NVL(S.当年送品05,0) AS 当年送品05                        
              ,NVL(S.当年送品06,0) AS 当年送品06                        
              ,NVL(S.当年送品07,0) AS 当年送品07                        
              ,NVL(S.当年送品08,0) AS 当年送品08                        
              ,NVL(S.当年送品09,0) AS 当年送品09                        
              ,NVL(S.当年送品10,0) AS 当年送品10                        
              ,NVL(S.当年送品11,0) AS 当年送品11                        
              ,NVL(S.当年送品12,0) AS 当年送品12                        
              ,NVL(S.当年送品上期,0) AS 当年送品上期                    
              ,NVL(S.当年送品下期,0) AS 当年送品下期                    
              ,NVL(S.当年送品累計,0) AS 当年送品累計                    
              ,NVL(S.当年純送01,0) AS 当年純送01                        
              ,NVL(S.当年純送02,0) AS 当年純送02                        
              ,NVL(S.当年純送03,0) AS 当年純送03                        
              ,NVL(S.当年純送04,0) AS 当年純送04                        
              ,NVL(S.当年純送05,0) AS 当年純送05                        
              ,NVL(S.当年純送06,0) AS 当年純送06                        
              ,NVL(S.当年純送07,0) AS 当年純送07                        
              ,NVL(S.当年純送08,0) AS 当年純送08                        
              ,NVL(S.当年純送09,0) AS 当年純送09                        
              ,NVL(S.当年純送10,0) AS 当年純送10                        
              ,NVL(S.当年純送11,0) AS 当年純送11                        
              ,NVL(S.当年純送12,0) AS 当年純送12                        
              ,NVL(S.当年純送上期,0) AS 当年純送上期                    
              ,NVL(S.当年純送下期,0) AS 当年純送下期                    
              ,NVL(S.当年純送累計,0) AS 当年純送累計                    
              ,NVL(S.当年Ａ返01,0) AS 当年Ａ返01                        
              ,NVL(S.当年Ａ返02,0) AS 当年Ａ返02                        
              ,NVL(S.当年Ａ返03,0) AS 当年Ａ返03                        
              ,NVL(S.当年Ａ返04,0) AS 当年Ａ返04                        
              ,NVL(S.当年Ａ返05,0) AS 当年Ａ返05                        
              ,NVL(S.当年Ａ返06,0) AS 当年Ａ返06                        
              ,NVL(S.当年Ａ返07,0) AS 当年Ａ返07                        
              ,NVL(S.当年Ａ返08,0) AS 当年Ａ返08                        
              ,NVL(S.当年Ａ返09,0) AS 当年Ａ返09                        
              ,NVL(S.当年Ａ返10,0) AS 当年Ａ返10                        
              ,NVL(S.当年Ａ返11,0) AS 当年Ａ返11                        
              ,NVL(S.当年Ａ返12,0) AS 当年Ａ返12                        
              ,NVL(S.当年Ａ返上期,0) AS 当年Ａ返上期                    
              ,NVL(S.当年Ａ返下期,0) AS 当年Ａ返下期                    
              ,NVL(S.当年Ａ返累計,0) AS 当年Ａ返累計                    
              ,NVL(S.当年Ｂ返01,0) AS 当年Ｂ返01                        
              ,NVL(S.当年Ｂ返02,0) AS 当年Ｂ返02                        
              ,NVL(S.当年Ｂ返03,0) AS 当年Ｂ返03                        
              ,NVL(S.当年Ｂ返04,0) AS 当年Ｂ返04                        
              ,NVL(S.当年Ｂ返05,0) AS 当年Ｂ返05                        
              ,NVL(S.当年Ｂ返06,0) AS 当年Ｂ返06                        
              ,NVL(S.当年Ｂ返07,0) AS 当年Ｂ返07                        
              ,NVL(S.当年Ｂ返08,0) AS 当年Ｂ返08                        
              ,NVL(S.当年Ｂ返09,0) AS 当年Ｂ返09                        
              ,NVL(S.当年Ｂ返10,0) AS 当年Ｂ返10                        
              ,NVL(S.当年Ｂ返11,0) AS 当年Ｂ返11                        
              ,NVL(S.当年Ｂ返12,0) AS 当年Ｂ返12                        
              ,NVL(S.当年Ｂ返上期,0) AS 当年Ｂ返上期                    
              ,NVL(S.当年Ｂ返下期,0) AS 当年Ｂ返下期                    
              ,NVL(S.当年Ｂ返累計,0) AS 当年Ｂ返累計                    
              ,NVL(S.当年解返01,0) AS 当年解返01                        
              ,NVL(S.当年解返02,0) AS 当年解返02                        
              ,NVL(S.当年解返03,0) AS 当年解返03                        
              ,NVL(S.当年解返04,0) AS 当年解返04                        
              ,NVL(S.当年解返05,0) AS 当年解返05                        
              ,NVL(S.当年解返06,0) AS 当年解返06                        
              ,NVL(S.当年解返07,0) AS 当年解返07                        
              ,NVL(S.当年解返08,0) AS 当年解返08                        
              ,NVL(S.当年解返09,0) AS 当年解返09                        
              ,NVL(S.当年解返10,0) AS 当年解返10                        
              ,NVL(S.当年解返11,0) AS 当年解返11                        
              ,NVL(S.当年解返12,0) AS 当年解返12                        
              ,NVL(S.当年解返上期,0) AS 当年解返上期                    
              ,NVL(S.当年解返下期,0) AS 当年解返下期                    
              ,NVL(S.当年解返累計,0) AS 当年解返累計                    
              ,NVL(S.当年純入金01,0) AS 当年純入金01                    
              ,NVL(S.当年純入金02,0) AS 当年純入金02                    
              ,NVL(S.当年純入金03,0) AS 当年純入金03                    
              ,NVL(S.当年純入金04,0) AS 当年純入金04                    
              ,NVL(S.当年純入金05,0) AS 当年純入金05                    
              ,NVL(S.当年純入金06,0) AS 当年純入金06                    
              ,NVL(S.当年純入金07,0) AS 当年純入金07                    
              ,NVL(S.当年純入金08,0) AS 当年純入金08                    
              ,NVL(S.当年純入金09,0) AS 当年純入金09                    
              ,NVL(S.当年純入金10,0) AS 当年純入金10                    
              ,NVL(S.当年純入金11,0) AS 当年純入金11                    
              ,NVL(S.当年純入金12,0) AS 当年純入金12                    
              ,NVL(S.当年純入金上期,0) AS 当年純入金上期                
              ,NVL(S.当年純入金下期,0) AS 当年純入金下期                
              ,NVL(S.当年純入金累計,0) AS 当年純入金累計                
              ,NVL(S.当年責任額01,0) AS 当年責任額01                    
              ,NVL(S.当年責任額02,0) AS 当年責任額02                    
              ,NVL(S.当年責任額03,0) AS 当年責任額03                    
              ,NVL(S.当年責任額04,0) AS 当年責任額04                    
              ,NVL(S.当年責任額05,0) AS 当年責任額05                    
              ,NVL(S.当年責任額06,0) AS 当年責任額06                    
              ,NVL(S.当年責任額07,0) AS 当年責任額07                    
              ,NVL(S.当年責任額08,0) AS 当年責任額08                    
              ,NVL(S.当年責任額09,0) AS 当年責任額09                    
              ,NVL(S.当年責任額10,0) AS 当年責任額10                    
              ,NVL(S.当年責任額11,0) AS 当年責任額11                    
              ,NVL(S.当年責任額12,0) AS 当年責任額12                    
              ,NVL(S.当年責任額上期,0) AS 当年責任額上期                
              ,NVL(S.当年責任額下期,0) AS 当年責任額下期                
              ,NVL(S.当年責任額累計,0) AS 当年責任額累計                
             FROM                                                       
              (                                                         
              SELECT                                                    
                 T1.得意先コード枝親                                    
                ,T1.内部ＢＫ||'0' AS 内部ＢＫ0                          
                ,T2.ブランドコード                                      
                ,T2.取引状態                                            
                ,T2.取引年月日                                          
                ,T2.解約年月日                                          
                ,T1.取引状態総合                                        
                ,T1.取引年月日総合                                      
                ,T1.解約年月日総合                                      
                ,T1.チャネル区分                                        
                ,T1.新チャネル区分                                      
                ,T1.ＳＲ区分                                            
                ,T1.クラスコード                                        
                ,T1.業態２区分                                          
                ,T1.領収書区分                                          
                ,T1.割戻相殺                                            
                ,T3.入金方法                                            
                ,T3.支払条件定義キー                                    
                ,T3.自動引落対象                                        
                ,T3.請求書フォーマット                                  
                                                                        
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) =                  
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) BETWEEN            
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) =                  
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度新規区分         
                ,CASE WHEN SUBSTR(T2.取引年月日,1,6) BETWEEN            
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計新規区分         
                                                                        
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) =              
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) BETWEEN        
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) =              
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度新規区分総合     
                ,CASE WHEN SUBSTR(T1.取引年月日総合,1,6) BETWEEN        
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計新規区分総合     
                                                                        
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) =                  
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) BETWEEN            
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) =                  
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度解約区分         
                ,CASE WHEN SUBSTR(T2.解約年月日,1,6) BETWEEN            
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計解約区分         
                                                                        
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) =              
                      :WK前年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 前年月度解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) BETWEEN        
                      :WK前年抽出開始年月 AND :WK前年抽出終了年月       
                      THEN '1' ELSE '0' END AS 前年累計解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) =              
                      :WK当年抽出終了年月                               
                      THEN '1' ELSE '0' END AS 当年月度解約区分総合     
                ,CASE WHEN SUBSTR(T1.解約年月日総合,1,6) BETWEEN        
                      :WK当年抽出開始年月 AND :WK当年抽出終了年月       
                      THEN '1' ELSE '0' END AS 当年累計解約区分総合     
               FROM                                                     
                  HAT_TOKUI_KIHON  T1                                   
                , HCV_TOKUI_BRAND_CVS  T2                               
                , HAT_TOKUI_SYOSAI T3                                   
               WHERE                                                    
                     T1.対象年月 = :WK対象年月度                        
                 AND T1.対象ＡＢ区分 = :WK対象ＡＢ区分                  
                 AND T1.得意先コード = T1.得意先コード枝親              
                 AND T2.得意先コード = T2.得意先コード枝親              
                 AND T3.得意先コード = T3.得意先コード枝親           
                 AND T2.ブランドコード NOT IN ('02', '20', '21', '23',
                                               '13', '66','30',
                                               '80', '81', '82')        
      *           AND T2.取引状態 IN ('0', '1', '2', '3')                 
                 AND T1.得意先コード = T2.得意先コード                  
                 AND T1.対象年月 = T2.対象年月                          
                 AND T1.対象ＡＢ区分 = T2.対象ＡＢ区分                  
                 AND T1.得意先コード = T3.得意先コード                  
                 AND T1.対象年月 = T3.対象年月                          
                 AND T1.対象ＡＢ区分 = T3.対象ＡＢ区分                  
              ) T                                                       
             ,(                                                         
              SELECT                                                    
                 S1.得意先コード枝親                                    
                ,NVL(D1.ブランド変換コード,S1.ブランドコード)           
                  AS ブランドコード                                     
       /* 前年送品 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品01                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品02                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品03                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品04                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品05                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品06                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品07                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品08                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品09                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                   THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END) 
                  AS 前年送品10                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品11                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品12                                         
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品上期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品下期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 前年送品累計                                       
                                                                        
       /* 前年純送 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 前年純送累計        
                                                                        
       /* 前年Ａ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 前年Ａ返累計        
                                                                        
       /* 前年Ｂ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 前年Ｂ返累計        
                                                                        
       /* 前年解返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 前年解返累計      
                                                                        
       /* 前年純入金 */                                                 
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'01'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'02'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'03'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'04'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'05'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'06'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'07'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'08'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度||'09'        
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'10'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'11'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK前年度 ||'12'     
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度||'06'                
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'07'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK前年度||'01'       
                  AND S1.暦月処理年月 <= :WK前年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 前年純入金累計      
                                                                        
       /* 前年責任額 */                                                 
                ,'0' AS 前年責任額01                                    
                ,'0' AS 前年責任額02                                    
                ,'0' AS 前年責任額03                                    
                ,'0' AS 前年責任額04                                    
                ,'0' AS 前年責任額05                                    
                ,'0' AS 前年責任額06                                    
                ,'0' AS 前年責任額07                                    
                ,'0' AS 前年責任額08                                    
                ,'0' AS 前年責任額09                                    
                ,'0' AS 前年責任額10                                    
                ,'0' AS 前年責任額11                                    
                ,'0' AS 前年責任額12                                    
                ,'0' AS 前年責任額上期                                  
                ,'0' AS 前年責任額下期                                  
                ,'0' AS 前年責任額累計                                  
                                                                        
       /* 当年送品 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品01                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品02                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品03                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品04                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品05                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品06                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品07                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品08                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品09                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品10                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品11                                         
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品12                                         
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品上期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品下期                                       
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額+Ａ返品金額+Ｂ返品金額 ELSE 0 END)  
                  AS 当年送品累計                                       
                                                                        
       /* 当年純送 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純送品金額 ELSE 0 END) AS 当年純送累計        
                                                                        
       /* 当年Ａ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ａ返品金額 ELSE 0 END) AS 当年Ａ返累計        
                                                                        
       /* 当年Ｂ返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返01          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返02          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返03          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返04          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返05          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返06          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返07          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返08          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返09          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返10          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返11          
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返12          
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返上期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返下期        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.Ｂ返品金額 ELSE 0 END) AS 当年Ｂ返累計        
                                                                        
       /* 当年解返 */                                                   
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.解約返品金額 ELSE 0 END) AS 当年解返累計      
                                                                        
       /* 当年純入金 */                                                 
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'01'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金01        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'02'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金02        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'03'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金03        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'04'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金04        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'05'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金05        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'06'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金06        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'07'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金07        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'08'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金08        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度||'09'        
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金09        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'10'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金10        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'11'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金11        
                ,SUM(CASE WHEN S1.暦月処理年月 = :WK当年度 ||'12'     
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金12        
                                                                        
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度||'06'                
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金上期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'07'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金下期      
                ,SUM(CASE WHEN S1.暦月処理年月 >= :WK当年度||'01'       
                  AND S1.暦月処理年月 <= :WK当年度 ||'12'             
                  THEN S1.純入金金額 ELSE 0 END) AS 当年純入金累計      
                                                                        
       /* 当年責任額 */                                                 
                ,'0' AS 当年責任額01                                    
                ,'0' AS 当年責任額02                                    
                ,'0' AS 当年責任額03                                    
                ,'0' AS 当年責任額04                                    
                ,'0' AS 当年責任額05                                    
                ,'0' AS 当年責任額06                                    
                ,'0' AS 当年責任額07                                    
                ,'0' AS 当年責任額08                                    
                ,'0' AS 当年責任額09                                    
                ,'0' AS 当年責任額10                                    
                ,'0' AS 当年責任額11                                    
                ,'0' AS 当年責任額12                                    
                ,'0' AS 当年責任額上期                                  
                ,'0' AS 当年責任額下期                                  
                ,'0' AS 当年責任額累計                                  
                                                                        
               FROM                                                     
                  HCT_SALES_PERFORMANCE  S1                             
                ,(SELECT '02' AS ブランドコード                         
                        ,'01' AS ブランド変換コード FROM dual           
                  UNION ALL                                             
                  SELECT '20','19' FROM dual                            
                  UNION ALL                                             
                  SELECT '21','10' FROM dual                            
       /* 2011.10.20 ADD ﾎﾟｰﾙ追加 */   
                  UNION ALL                                             
                  SELECT '66','65' FROM dual                            
       /* 2011.11.17 ADD CVS50追加 */   
                  UNION ALL                                             
                  SELECT '30','12' FROM dual                            
                  UNION ALL                                             
                  SELECT '23','22' FROM dual                            
                 ) D1                                                   
               WHERE                                                    
                     S1.ブランドコード = D1.ブランドコード (+)          
                 AND S1.ブランドコード NOT IN ('90', '91', '92')        
                 AND (  (   S1.暦月処理年月 >= :WK前年抽出開始年月      
                        AND S1.暦月処理年月 <= :WK前年抽出終了年月)     
                     OR (   S1.暦月処理年月 >= :WK当年抽出開始年月      
                        AND S1.暦月処理年月 <= :WK当年抽出終了年月) )   
               GROUP BY                                                 
                     S1.得意先コード枝親                                
                    ,NVL(D1.ブランド変換コード,S1.ブランドコード)       
              ) S                                                       
                                                                        
             WHERE                                                      
                   T.得意先コード枝親 = S.得意先コード枝親 (+)          
               AND T.ブランドコード = S.ブランドコード (+)              
                                                                        
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  INSERT-CNT.                            
                                                                        
           MOVE '台帳追加処理２完了' TO D-メッセージ情報.               
           CALL KXU002SC   USING  D-ログ情報.                           
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORI.                                                      
      *-----------------------------------------------------------------
           MOVE  'ＳＱＬエラー 台帳追加処理' TO D-メッセージ情報.       
           CALL  KXU002SC  USING  D-ログ情報.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-メッセージ情報.           
           MOVE SQLERRMC TO D-メッセージ情報(25:100).                   
           CALL KXU002SC USING D-ログ情報.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       終了処理.                                                        
      *-----------------------------------------------------------------
           EXEC SQL AT :DB-NAME COMMIT WORK RELEASE END-EXEC.           
           MOVE 'DELETE   ='   TO D-カウント見出し                      
           MOVE DELETE-CNT     TO D-カウント                            
           CALL KXU002SC    USING D-ログ情報                            
           MOVE 'INSERT   ='   TO D-カウント見出し                      
           MOVE INSERT-CNT     TO D-カウント                            
           CALL KXU002SC    USING D-ログ情報                            
           MOVE 'END       '   TO D-カウント見出し.                     
           CALL KXU002SC   USING  D-ログ情報.                           
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
