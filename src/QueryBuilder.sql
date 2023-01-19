-- =============================================
--				Q-Framework
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-15 10:00
-- Update date:	2023-01-18 15:00
-- Version: 	1.04
-- Description:	Framework for working with queries
--				in dynamic SQL
-- =============================================
GO
;IF SCHEMA_ID('q') IS NULL EXEC('CREATE SCHEMA q')
GO
;IF DATABASE_PRINCIPAL_ID('SM_sys_All') IS NOT NULL BEGIN
	;EXEC('GRANT EXECUTE ON SCHEMA::q TO [SM_sys_All]')
	;EXEC('GRANT SELECT ON SCHEMA::q TO [SM_sys_All]')
;END
GO
;DECLARE @ver 		FLOAT	= 1.0
;DECLARE @cur_ver	FLOAT	= 0

;DECLARE @strVerString	NVARCHAR(255)	= '-- Version:'
;IF OBJECT_ID('q.sys_proc_ver') IS NOT NULL BEGIN
	;DECLARE @strDefinition	NVARCHAR(MAX) = OBJECT_DEFINITION(OBJECT_ID('q.sys_proc_ver'))
	;DECLARE @start INT = CHARINDEX(@strVerString,@strDefinition)
	;IF @start>0 BEGIN
		;SET @strDefinition = SUBSTRING(@strDefinition,@start+LEN(@strVerString),LEN(@strDefinition))
		;DECLARE @strVersion	NVARCHAR(255) = REPLACE(REPLACE(REPLACE(REPLACE(
			SUBSTRING(@strDefinition,1,CHARINDEX(CHAR(13),@strDefinition))
			,CHAR(9),''),CHAR(10),''),CHAR(13),''),' ','')
		;SET @cur_ver = IIF(ISNUMERIC(@strVersion)=1,CAST(@strVersion AS FLOAT),0)
	;END
	;IF @cur_ver < @ver DROP FUNCTION q.sys_proc_ver
;END
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-14 02:07
-- Version: 	1.0
-- Description:	Get procedure/function current version
-- =============================================
CREATE FUNCTION q.sys_proc_ver(
	 @strProcName	SYSNAME
	,@strSchemaName	SYSNAME	= 'q'
)
RETURNS FLOAT
AS BEGIN
	;DECLARE @ver 	FLOAT	= 0
	;DECLARE @strVerString	NVARCHAR(255)	= '-- Version:'
	;IF OBJECT_ID(@strSchemaName+'.'+@strProcName) IS NOT NULL BEGIN
		;DECLARE @strDefinition	NVARCHAR(MAX) = OBJECT_DEFINITION(OBJECT_ID(@strSchemaName+'.'+@strProcName))
		;DECLARE @start INT = CHARINDEX(@strVerString,@strDefinition)
		;IF @start>0 BEGIN
			;SET @strDefinition = SUBSTRING(@strDefinition,@start+LEN(@strVerString),LEN(@strDefinition))
			;DECLARE @strVersion	NVARCHAR(255) = REPLACE(REPLACE(REPLACE(REPLACE(
				SUBSTRING(@strDefinition,1,CHARINDEX(CHAR(13),@strDefinition))
				,CHAR(9),''),CHAR(10),''),CHAR(13),''),' ','')
			;SET @ver = IIF(ISNUMERIC(@strVersion)=1,CAST(@strVersion AS FLOAT),0)
		;END
	;END
	;RETURN @ver
END
GO
;IF 1.0 > q.sys_proc_ver('sys_proc_drop',DEFAULT)
	IF OBJECT_ID('q.sys_proc_drop') IS NOT NULL
		DROP PROCEDURE q.sys_proc_drop
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 20:43
-- Version: 	1.0
-- Description:	Drop procedure or function with
--				lower version
-- =============================================
CREATE PROCEDURE q.sys_proc_drop
	 @cur_version	FLOAT
	,@proc_name		SYSNAME
AS BEGIN
	;IF @cur_version > q.sys_proc_ver(@proc_name,DEFAULT) BEGIN
		;IF OBJECT_ID('q.'+@proc_name) IS NOT NULL BEGIN
			;DECLARE @strQuery	NVARCHAR(MAX) = 'DROP ? [q].['+@proc_name+']'
			;SET @strQuery	= REPLACE(@strQuery,'?',(
				SELECT IIF([type]='P','PROCEDURE',IIF([type] IN ('TF','FT','AF','FN'),'FUNCTION',NULL))
				FROM sys.objects
				WHERE	[name]		= @proc_name
					AND	[schema_id]	= SCHEMA_ID('q')
			))
			;IF @strQuery IS NOT NULL
				EXEC (@strQuery)
		;END
	;END
END
GO
;EXEC q.sys_proc_drop 1.0,'sys_proc'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 20:43
-- Version: 	1.0
-- Description:	Return procedure full path
-- =============================================
CREATE FUNCTION q.sys_proc(
	 @intProcID	INT
)
RETURNS NVARCHAR(MAX)
WITH EXECUTE AS OWNER AS BEGIN
	RETURN ISNULL(DB_NAME()+'.'+OBJECT_SCHEMA_NAME(@intProcID)+'.'+OBJECT_NAME(@intProcID),CONCAT('Undefined Process ID=',@intProcID))
END
GO
;EXEC q.sys_proc_drop 1.0,'sys_xml2var'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-07 01:43
-- Version: 	1.0
-- Description:	Replace XML escaped characters
-- =============================================
CREATE FUNCTION q.sys_xml2var(
	@string	NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;SET @string = REPLACE(@string, '&amp;', '&')
	;SET @string = REPLACE(@string, '&lt;', '<')
	;SET @string = REPLACE(@string, '&gt;', '>')
	;SET @string = REPLACE(@string, '&apos;', '''')
	;SET @string = REPLACE(@string, '&quot;', '"')
	;SET @string = REPLACE(@string, '&#x0D;', CHAR(13))
	;RETURN @string
;END
GO
;EXEC q.sys_proc_drop 1.0,'_split'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 22:14
-- Version: 	1.0
-- Description:	Split string with separator in table
-- =============================================
CREATE FUNCTION q._split(
	 @list       NVARCHAR(MAX)
	,@delimiter  NVARCHAR(255)	= ','
)
RETURNS @return TABLE (
	 [number]	INT
	,[item]		NVARCHAR(MAX)
)
AS BEGIN
	;IF TRY_CONVERT(INT,@delimiter) IS NOT NULL AND @delimiter NOT IN (' ')
		INSERT INTO @return([Number],[Item])
		SELECT	 [number]	= ROW_NUMBER() OVER (ORDER BY [number])
				,[Item]		= [Item]
		FROM (
			SELECT
				 [number]
				,[Item]		= SUBSTRING(@list,1+([number]-1)*CAST(@delimiter AS INT),CAST(@delimiter AS INT))
			FROM (
				SELECT ROW_NUMBER() OVER (ORDER BY s1.[object_id])
				FROM		sys.all_objects AS s1
				CROSS APPLY	sys.all_objects
			) AS n([number])
			WHERE [number] <= CEILING(CAST(LEN(@list) AS FLOAT)/CAST(@delimiter AS INT))
		) AS y
	ELSE
		INSERT INTO @return([Number],[Item])
		SELECT
			 [number]	= ROW_NUMBER() OVER (ORDER BY [number])
			,[Item]		= [Item]
		FROM (
			SELECT
				 [number]
				,[Item]		=
						SUBSTRING(@list,[number]
							,CHARINDEX(@delimiter, @list + @delimiter, [number]) - [number]
						)
			FROM (
				SELECT ROW_NUMBER() OVER (ORDER BY s1.[object_id])
				FROM		sys.all_objects AS s1
				CROSS APPLY	sys.all_objects
			) AS n([number])
			WHERE
					[number]	<= CONVERT(INT, LEN(@list))
				AND	@delimiter	= SUBSTRING(@delimiter + @list, [number], LEN(@delimiter+'x')-1)
		) AS y

	;RETURN
;END
GO
;EXEC q.sys_proc_drop 1.0,'_split_row'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-26 19:50
-- Version: 	1.0
-- Description:	Split string with separator in table
-- =============================================
CREATE FUNCTION q._split_row(
	 @list      NVARCHAR(MAX)
	,@delimiter NVARCHAR(255)	= ','
)
RETURNS @return TABLE (
	 [item]		NVARCHAR(MAX)
	,[count]	INT
	,[val_01]	NVARCHAR(MAX)
	,[val_02]	NVARCHAR(MAX)
	,[val_03]	NVARCHAR(MAX)
	,[val_04]	NVARCHAR(MAX)
	,[val_05]	NVARCHAR(MAX)
	,[val_06]	NVARCHAR(MAX)
	,[val_07]	NVARCHAR(MAX)
	,[val_08]	NVARCHAR(MAX)
	,[val_09]	NVARCHAR(MAX)
	,[val_10]	NVARCHAR(MAX)
	,[other]	NVARCHAR(MAX)
)
AS BEGIN
	WITH SPLT ([item],[xml]) AS (
		SELECT @list,CONVERT(XML,'<items><item>'+REPLACE(@list,@delimiter,'</item><item>')+'</item></items>')
	)
	INSERT INTO @return
	SELECT *
				,[other]	= IIF([count]<=10,NULL,
						STUFF((
							SELECT @delimiter+[item]
							FROM q._split(@list,@delimiter)
							WHERE [number]>10
							FOR XML PATH('')
						,TYPE).value('(.)','NVARCHAR(MAX)'),1,LEN(@delimiter),'')
					)
	FROM (
		SELECT	 [item]
				,[count]	= xml.value('count(/items[1]/*)', 'int')
				,[val_01]	= xml.value('/items[1]/item[1]','NVARCHAR(MAX)')
				,[val_02]	= xml.value('/items[1]/item[2]','NVARCHAR(MAX)')
				,[val_03]	= xml.value('/items[1]/item[3]','NVARCHAR(MAX)')
				,[val_04]	= xml.value('/items[1]/item[4]','NVARCHAR(MAX)')
				,[val_05]	= xml.value('/items[1]/item[5]','NVARCHAR(MAX)')
				,[val_06]	= xml.value('/items[1]/item[6]','NVARCHAR(MAX)')
				,[val_07]	= xml.value('/items[1]/item[7]','NVARCHAR(MAX)')
				,[val_08]	= xml.value('/items[1]/item[8]','NVARCHAR(MAX)')
				,[val_09]	= xml.value('/items[1]/item[9]','NVARCHAR(MAX)')
				,[val_10]	= xml.value('/items[1]/item[10]','NVARCHAR(MAX)')

		FROM SPLT
	) Q

	;RETURN
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_flow_next'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 22:14
-- Version: 	1.0
-- Description:	Get next unique ID
-- =============================================
CREATE FUNCTION q.sys_flow_next()
RETURNS BIGINT
WITH EXECUTE AS OWNER
AS BEGIN
	RETURN [SM].[system].[ufn_GetScalarFromQuery]('SELECT (NEXT VALUE FOR [SM].[system].[flow]) AS Q', 0)
END
GO
;EXEC q.sys_proc_drop 1.0,'sys_flow_name'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 22:14
-- Version: 	1.0
-- Description:	Get next unique ID
-- =============================================
CREATE FUNCTION q.sys_flow_name(
	 @strParent		NVARCHAR(MAX)
	,@isConverted	INT				= 0
)
RETURNS NVARCHAR(MAX)
WITH EXECUTE AS OWNER
AS
BEGIN
	;RETURN (CAST((
		SELECT	 [name]			= ISNULL(@strParent,'null')
				,[datetime]		= CONVERT(NVARCHAR,GETDATE(),127)
				,[flow_num]		= q.sys_flow_next()
				,[is_converted]	= IIF(@isConverted=0,'false','true')
		FOR XML RAW('flow')
	) AS NVARCHAR(MAX)))
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_flow_check'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 22:14
-- Version: 	1.0
-- Description:	Build XML-type info string
-- =============================================
CREATE FUNCTION q.sys_flow_check(
	@strParent	NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
WITH EXECUTE AS OWNER
AS BEGIN
	;RETURN (q.sys_xml2var(CAST((
		SELECT IIF(LEFT([item],1)='<' AND RIGHT([item],2)='/>'
				,[item]
				,q.sys_flow_name(IIF(ISNUMERIC([item])=1,q.sys_proc([item]),[item]),1)
			)
		FROM q._split(REPLACE(@strParent,'/>','/>;'),';')
		ORDER BY [number]
		FOR XML PATH('')
	) AS NVARCHAR(MAX))))
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_stack'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-07-28 19:38
-- Version: 	1.0
-- Description:	Creating stack info string
-- =============================================
CREATE FUNCTION q.sys_stack(
	  @strParentPrev	NVARCHAR(MAX)
	 ,@strParentCur		NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
WITH EXECUTE AS OWNER
AS BEGIN
	;RETURN (
		ISNULL(q.sys_flow_check(@strParentPrev),'')
		+q.sys_flow_name(IIF(
			 ISNUMERIC(@strParentCur)=1
			,q.sys_proc(@strParentCur)
			,@strParentCur
		), 0)
	)
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_debug_start'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 20:50
-- Version: 	1.0
-- Description:	System procedure for debugging
-- =============================================
CREATE PROCEDURE q.sys_debug_start
--~
	 @intProcID		INT
	,@intDebug		INT				= 0		OUTPUT
	,@strParent		NVARCHAR(MAX)	= ''	OUTPUT
--~
AS BEGIN
	;DECLARE	@strType	SYSNAME
		
	;SET @strType =LOWER((SELECT [type_desc] FROM sys.objects WITH (NOLOCK) WHERE [object_id] = @intProcID))
	;SET @strParent	= q.sys_stack(@strParent,@intProcID)
	;IF @intDebug <> 0 PRINT q.sys_debug(@strType,@intDebug,@strParent,IIF(@intDebug BETWEEN 1 AND 100,OBJECT_DEFINITION(@intProcID),NULL))
	;IF @intDebug <> 0 SET @intDebug = @intDebug*IIF(@intDebug BETWEEN 1 AND 100,1000,1)+1
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_debug_end'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-05 20:42
-- Version: 	1.0
-- Description:	System function for debugging
-- =============================================
CREATE FUNCTION q.sys_debug_end(
	 @intProcID		INT
	,@intDebug		INT
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;DECLARE	@tabf		NVARCHAR(100)	= REPLICATE(CHAR(9),IIF(@intDebug<1000,0,@intDebug-1000-1))
		;DECLARE	@strType	SYSNAME
		
		;SET @strType = LOWER((SELECT [type_desc] FROM sys.objects WITH (NOLOCK) WHERE [object_id] = @intProcID))

	-- RETURN
		;RETURN IIF(@intDebug=0,NULL,N'--=≡'+@tabf+'</'+@strType+'>')
;END
GO
;EXEC q.sys_proc_drop 1.01,'sys_debug'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-29 18:08
-- Update date: 2022-12-26 21:54
-- Version: 	1.01
-- Description:	Show debug info
-- =============================================
CREATE FUNCTION q.sys_debug(
	 @strType		NVARCHAR(MAX)
	,@intDebug		INT
	,@strParent		NVARCHAR(MAX)
	,@strDefinition	NVARCHAR(MAX)	= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;DECLARE	@i		INT
		;DECLARE	@br		CHAR(2)			= CHAR(13)+CHAR(10)
		;DECLARE	@tab	CHAR(1)			= CHAR(9)
		;DECLARE	@tabf	NVARCHAR(100)	= REPLICATE(@tab,IIF(@intDebug<1000,0,@intDebug-1000))
		;DECLARE	@last	NVARCHAR(255)
		;DECLARE	@return	NVARCHAR(MAX)	= '(#1)(#2)(#3)'
		;DECLARE	@start	NVARCHAR(MAX)
		;DECLARE	@def	NVARCHAR(MAX)
		;DECLARE	@main	NVARCHAR(MAX)
		;DECLARE	@stack	NVARCHAR(MAX)

	-- FUNCTION START
		;SET @strDefinition = IIF(@intDebug BETWEEN 1 AND 100,@strDefinition,NULL)
		;SET @strParent	= q.sys_flow_check(@strParent)

		;SET @stack =	@br+N'--=≡'+@tabf+@tab+'<call_stack>'+
						REPLACE(@strParent,@br,@br+N'--=≡'+@tabf+@tab+@tab)+
						@br+N'--=≡'+@tabf+@tab+'</call_stack>'

		;SELECT @last = REPLACE(REPLACE(x.value('@name','NVARCHAR(255)'),'[',''),']','')
		FROM (SELECT CAST('<a>'+@strParent+'</a>' AS XML)) a(b)
		CROSS APPLY b.nodes('/a/flow[position()=last()]') c(x)

		;SET @main		=	N'--=≡'+@tabf+'<{TYPE} debug_level="{DEBUG}" name="{NAME}">{CALLER}'+@br

		;IF @strDefinition IS NOT NULL BEGIN
			;SET @i		=	CHARINDEX('CREATE '+@strType,@strDefinition) - 3
			;SET @start	=	N'--=≡<?xml version="1.0" encoding="utf-8"?>'+@br+
							N'--=≡<!DOCTYPE lemex_sql_debug>'+@br
			;SET @def	=	N'--=≡	<info type="lemex_sql_debug" user_name="{USER_NAME}" datetime="{DATETIME}" host="{HOST}">'+@br+
							N'--=≡		<definition>'+@br+
							N'--=≡			<![CDATA['+@br+
							LEFT(@strDefinition,IIF(@i>0,@i,0))+@br+
							N'--=≡			]]>'+@br+
							N'--=≡		</definition>'+@br+
							N'--=≡	</info>'+@br
		
			;SET @return = REPLACE(@return,	'(#1)',	@start)
			;SET @return = REPLACE(@return,	'(#3)',	@def)

			;SET @return = REPLACE(@return,	'{USER_NAME}',	USER_NAME())
			;SET @return = REPLACE(@return,	'{DATETIME}',	CONVERT(NVARCHAR,GETDATE(),126))
			;SET @return = REPLACE(@return,	'{HOST}',		HOST_NAME())
		;END ELSE BEGIN
			SET @return = REPLACE(@return,	'(#1)',	'')
			SET @return = REPLACE(@return,	'(#3)',	'')
		;END

		
		;SET @main		= REPLACE(@main		,'{DEBUG}'	,CAST(@intDebug AS NVARCHAR))
		;SET @main		= REPLACE(@main		,'{CALLER}'	,ISNULL(@stack,''))
		;SET @main		= REPLACE(@main		,'{TYPE}'	,@strType)
		;SET @main		= REPLACE(@main		,'{NAME}'	,ISNULL(@last,''))
		;SET @return	= REPLACE(@return	,'(#2)'		,@main)

		;RETURN @return
	-- FUNCTION END
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_info'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2015-05-05 14:42
-- Version: 	1.0
-- Description:	Show debug info for query
-- =============================================
CREATE FUNCTION q.sys_info(
	 @strQuery		NVARCHAR(MAX)
	,@intDebug		INT
	,@strParent		NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;DECLARE	@br		CHAR(2)			= CHAR(13)+CHAR(10)
		;DECLARE	@tab	CHAR(1)			= CHAR(9)
		;DECLARE	@tabf	NVARCHAR(100)	= REPLICATE(@tab,IIF(@intDebug<1000,0,@intDebug-1000))

	-- FUNCTION START
		;RETURN q.sys_debug('execute',@intDebug,@strParent,NULL)+
				@br+N'--=≡'+@tabf+@tab+'<query>'+
				@br+N'--=≡'+@tabf+@tab+@tab+'<![CDATA['+
				@br+@strQuery+
				@br+N'--=≡'+@tabf+@tab+@tab+']]>'+
				@br+N'--=≡'+@tabf+@tab+'</query>'+
				@br+N'--=≡'+@tabf+'</execute>'
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_safe'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2017-03-25 10:44
-- Version: 	1.0
-- Description:	Query escaping
-- =============================================
CREATE FUNCTION q.sys_safe(
	  @strQuery		NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
AS BEGIN RETURN
	REPLACE(REPLACE(@strQuery, '''', ''''''), '`', '``')
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_quote'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2015-05-05 14:42
-- Version: 	1.0
-- Description:	Var quotation
-- =============================================
CREATE FUNCTION q.sys_quote(
	 @strVarValue	NVARCHAR(MAX)
	,@strVarIfNull	NVARCHAR(MAX)	= NULL
	,@strPreValue	NVARCHAR(MAX)	= NULL
	,@strPostValue	NVARCHAR(MAX)	= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN RETURN
	COALESCE(ISNULL(@strPreValue,'')+''''+q.sys_safe(@strVarValue)+''''+ISNULL(@strPostValue,'')
		,q.sys_safe(@strVarIfNull)
		,'NULL'
	)
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_replace'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2015-05-05 14:42
-- Version: 	1.0
-- Description:	Query escaping
-- =============================================
CREATE FUNCTION q.sys_replace(
	 @strQuery		NVARCHAR(MAX)
	,@strSign		NVARCHAR(5)	= '`'
	,@strReplace	NVARCHAR(5)	= ''''
	,@strEscape		NCHAR(1)	= '\'
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		DECLARE	@temp	NVARCHAR(255);

	-- FUNCTION START
		SET @temp		= '706A4115EF234CDF9C238BC2975CD48038CCFF66F27F4C60ABFDDE8F603A38C6';
		SET @strQuery	= REPLACE(@strQuery, @strEscape+@strSign, @temp);
		SET @strQuery	= REPLACE(@strQuery, @strSign, @strReplace);
		SET @strQuery	= REPLACE(@strQuery, @temp, @strEscape+@strSign);
	-- FUNCTION END

	-- RETURN
		RETURN @strQuery;
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_unquote'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-22 08:39
-- Version: 	1.0
-- Description:	Remove square brackets from var value
-- =============================================
CREATE FUNCTION q.sys_unquote(
	 @strVarValue	NVARCHAR(255)
)
RETURNS NVARCHAR(MAX)
AS BEGIN RETURN
	IIF(NOT (LEFT(@strVarValue,1)='[' AND RIGHT(@strVarValue,1)=']'),@strVarValue,
		REPLACE(SUBSTRING(@strVarValue,2,LEN(@strVarValue)-2),']]',']')
	)
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_inject'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-21 08:01
-- Version: 	1.0
-- Description:	Replace var in Query
-- =============================================
CREATE FUNCTION q.sys_inject(
	 @strQuery		NVARCHAR(MAX)
	,@strVarName	NVARCHAR(255)
	,@strVarValue	NVARCHAR(MAX)
	,@strVarType	NVARCHAR(255)
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;DECLARE	@br	CHAR(2)	= CHAR(13)+CHAR(10)
	;IF CHARINDEX(@strVarName,@strQuery,1)>0 BEGIN
		;SET @strQuery	= REPLACE(@strQuery,'[*'+@strVarName+'*]',REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ISNULL(@strVarValue,'NULL'),CHAR(13)+CHAR(10),CHAR(13)),CHAR(10)+CHAR(13),CHAR(13)),CHAR(10),CHAR(13)),CHAR(13),@br+'--'),'*/','* /'),'/*','/ *'))
		;SET @strQuery	= REPLACE(@strQuery,'[!'+@strVarName+'!]',ISNULL(@strVarValue,'NULL'))
		;SET @strQuery	= REPLACE(@strQuery,'[{'+@strVarName+'}]',ISNULL(QUOTENAME(q.sys_unquote(@strVarValue)),'NULL'))
		;SET @strQuery	= REPLACE(@strQuery,'["'+@strVarName+'"]',ISNULL(q.sys_safe(@strVarValue),'NULL'))
		;SET @strQuery	= REPLACE(@strQuery,'[#'+@strVarName+'#]',(
				CASE @strVarType
					WHEN 'NVARCHAR'	THEN q.sys_quote(@strVarValue,DEFAULT,'N',DEFAULT)
					WHEN 'GUID'		THEN q.sys_quote(@strVarValue,DEFAULT,'CAST(',' AS UNIQUEIDENTIFIER)')
					WHEN 'XML'		THEN q.sys_quote(@strVarValue,DEFAULT,'CAST(',' AS XML)')
					WHEN 'DATE'		THEN q.sys_quote(REPLACE(CONVERT(NVARCHAR,CAST(@strVarValue AS DATE),126),'-',''),DEFAULT,'CAST(',' AS DATE)')
					WHEN 'TIME'		THEN q.sys_quote(CONVERT(NVARCHAR,CAST(@strVarValue AS TIME),126),DEFAULT,'CAST(',' AS TIME)')
					WHEN 'DATETIME'	THEN q.sys_quote(
											 REPLACE(CONVERT(NVARCHAR,CAST(@strVarValue AS DATE),126),'-','')
											+' '+CONVERT(NVARCHAR,CAST(@strVarValue AS TIME),126)
										,DEFAULT,'CAST(',' AS DATETIME)')
					ELSE ISNULL(@strVarValue,'NULL')
				END
			))
	;END

	;RETURN @strQuery
END
GO
;EXEC q.sys_proc_drop 1.0,'_show'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-15 21:12
-- Version: 	1.0
-- Description:	Get all variables from XML data
-- =============================================
CREATE FUNCTION q._show(
	 @xml_vars	XML
)
RETURNS @vars		TABLE  (
	 [id]			INT NOT NULL
	,[name]			NVARCHAR(255) NOT NULL
	,[type]			SYSNAME DEFAULT('NVARCHAR') NOT NULL
	,[is_null]		INT NOT NULL
	,[datetime]		DATETIME
	,[value]		NVARCHAR(MAX) NULL
	,[var_nvarchar]	NVARCHAR(MAX)
	,[var_int]		INT
	,[var_bigint]	BIGINT
	,[var_float]	FLOAT
	,[var_real]		REAL
	,[var_numeric]	NUMERIC
	,[var_money]	MONEY
	,[var_date]		DATE
	,[var_time]		TIME
	,[var_datetime]	DATETIME
	,[var_xml]		XML
	,[var_guid]		UNIQUEIDENTIFIER
	,[var_bin]		VARBINARY(MAX)
)
AS BEGIN
	;INSERT INTO @vars(
		 [id]
		,[name]
		,[type]
		,[is_null]
		,[datetime]
		,[value]
		,[var_nvarchar]
		,[var_int]
		,[var_bigint]
		,[var_float]
		,[var_real]
		,[var_numeric]
		,[var_money]
		,[var_date]
		,[var_time]
		,[var_datetime]
		,[var_xml]
		,[var_guid]
		,[var_bin]
	)
	SELECT
		 [id]
		,[name]
		,[type]
		,[is_null]
		,[datetime]
		,[value]
		,[var_nvarchar]	= IIF([type]='NVARCHAR',[value],NULL)
		,[var_int]		= TRY_CONVERT(INT				,IIF([type]='INT'		,[value],NULL))
		,[var_bigint]	= TRY_CONVERT(BIGINT			,IIF([type]='BIGINT'	,[value],NULL))
		,[var_float]	= TRY_CONVERT(FLOAT				,IIF([type]='FLOAT'		,[value],NULL))
		,[var_real]		= TRY_CONVERT(REAL				,IIF([type]='REAL'		,[value],NULL))
		,[var_numeric]	= TRY_CONVERT(NUMERIC			,IIF([type]='NUMERIC'	,[value],NULL))
		,[var_money]	= TRY_CONVERT(MONEY				,IIF([type]='MONEY'		,[value],NULL))
		,[var_date]		= TRY_CONVERT(DATE				,IIF([type]='DATE'		,[value],NULL))
		,[var_time]		= TRY_CONVERT(TIME				,IIF([type]='TIME'		,[value],NULL))
		,[var_datetime]	= TRY_CONVERT(DATETIME			,IIF([type]='DATETIME'	,[value],NULL))
		,[var_xml]		= TRY_CONVERT(XML				,IIF([type]='XML'		,[value],NULL))
		,[var_guid]		= TRY_CONVERT(UNIQUEIDENTIFIER	,IIF([type]='GUID'		,[value],NULL))
		,[var_bin]		= TRY_CONVERT(VARBINARY(MAX)	,IIF([type]='BIN'		,[value],NULL),	1)
	FROM (
		SELECT *,[value]=IIF([is_null]=1,NULL,[_value]) FROM (
			SELECT
				 [id]		= x.value('@id[1]'			,'INT')
				,[name]		= x.value('@name[1]'		,'NVARCHAR(255)')
				,[type]		= x.value('@type[1]'		,'NVARCHAR(255)')
				,[is_null]	= x.value('@is_null[1]'		,'INT')
				,[datetime]	= x.value('@datetime[1]'	,'DATETIME')
				,[_value]	= x.value('.'				,'NVARCHAR(MAX)')
			FROM @xml_vars.nodes('/vars/item') t(x)
		) W
	) Q
	ORDER BY [id]

	;RETURN
;END
GO
;EXEC q.sys_proc_drop 1.0,'_input'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-21 07:48
-- Version: 	1.0
-- Description:	Replace in Query all variables from XML data
-- =============================================
CREATE FUNCTION q._input(
	 @strQuery	NVARCHAR(MAX)
	,@xml_vars	XML
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;DECLARE	@intVarID_1		INT
		;DECLARE	@strVarName_1	NVARCHAR(255)
		;DECLARE	@strVarType_1	NVARCHAR(255)
		;DECLARE	@strVarValue_1	NVARCHAR(MAX)

		;DECLARE	@intVarID_2		INT
		;DECLARE	@strVarName_2	NVARCHAR(255)
		;DECLARE	@strVarType_2	NVARCHAR(255)
		;DECLARE	@strVarValue_2	NVARCHAR(MAX)

		;DECLARE	@tblVars		TABLE(
					 [id]		INT
					,[name]		NVARCHAR(255)
					,[type]		NVARCHAR(255)
					,[value]	NVARCHAR(MAX)
				)

	-- FUNCTION START
		;INSERT INTO @tblVars([id],[name],[type],[value])
		SELECT [id],[name],[type],[value]
		FROM q._show(@xml_vars)
		ORDER BY [id]

		;DECLARE curA CURSOR FOR (SELECT * FROM @tblVars)
		;OPEN curA
		;FETCH NEXT FROM curA INTO @intVarID_1,@strVarName_1,@strVarType_1,@strVarValue_1
		;WHILE @@FETCH_STATUS = 0 BEGIN
			;DECLARE curB CURSOR FOR (SELECT * FROM @tblVars WHERE [name] <> @strVarName_1)
			;OPEN curB
			;FETCH NEXT FROM curB INTO @intVarID_2,@strVarName_2,@strVarType_2,@strVarValue_2
			;WHILE @@FETCH_STATUS = 0 BEGIN
				;SET @strVarValue_1	= q.sys_inject(@strVarValue_1,@strVarName_2,@strVarValue_2,@strVarType_2)

				;FETCH NEXT FROM curB INTO @intVarID_2,@strVarName_2,@strVarType_2,@strVarValue_2
			;END
			;CLOSE curB
			;DEALLOCATE curB

			;SET @strQuery	= q.sys_inject(@strQuery,@strVarName_1,@strVarValue_1,@strVarType_1)

			;FETCH NEXT FROM curA INTO @intVarID_1,@strVarName_1,@strVarType_1,@strVarValue_1
		;END
		;CLOSE curA
		;DEALLOCATE curA

	-- RETURN
	;RETURN @strQuery
;END
GO
;EXEC q.sys_proc_drop 1.01,'_get'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-21 07:48
-- Update date: 2022-12-26 18:23
-- Version: 	1.01
-- Description:	Get Query from procedure/function
--				definition or from local variables
--				table
-- =============================================
CREATE FUNCTION q._get(
	 @strQueryName			NVARCHAR(255)	= NULL
	,@strGroupNameOrProcID	NVARCHAR(255)	= NULL
	,@xmlVars				XML				= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;DECLARE @br				CHAR(2)			= CHAR(13)+CHAR(10)
	;DECLARE @strDefaultName	NVARCHAR(255)	= 'Query'
	;DECLARE @strDefinition		NVARCHAR(MAX)
	;DECLARE @strQuery			NVARCHAR(MAX)

	;DECLARE @strDBName		SYSNAME = ''
	;DECLARE @strSchema		SYSNAME = ''
	;DECLARE @strObjName	SYSNAME = ''	
	
	;IF ISNUMERIC(@strGroupNameOrProcID)=1 BEGIN
		-- User has sent OBJECT_ID
		;IF OBJECT_NAME(@strGroupNameOrProcID) IS NOT NULL BEGIN
			-- Object exists in Current DB
			;SET @strDefinition			= OBJECT_DEFINITION(@strGroupNameOrProcID)
			;SET @strGroupNameOrProcID	= q.sys_proc(@strGroupNameOrProcID)
		;END
	;END ELSE BEGIN
		-- Object exists in Current DB
		;IF OBJECT_NAME(OBJECT_ID(@strGroupNameOrProcID)) IS NOT NULL BEGIN
			;SET @strDefinition		= OBJECT_DEFINITION(OBJECT_ID(@strGroupNameOrProcID))
		;END ELSE BEGIN
			-- Try to find object in different DB
			;IF OBJECT_ID(@strGroupNameOrProcID) IS NOT NULL BEGIN
				;SELECT	 @strDBName		+= IIF(Q.[number]=1,q.sys_unquote([item]),'')
						,@strSchema		+= IIF(Q.[number]=2,q.sys_unquote([item]),'')
						,@strObjName	+= IIF(Q.[number]=3,q.sys_unquote([item]),'')
				FROM q._split(@strGroupNameOrProcID,'.') Q
	
				;SET @strDefinition		= [SM].[system].[ufn_GetObjDefinition](@strDBName,@strSchema,@strObjName)
			;END
		;END
	;END

	-- Try to get Query from Object Definition
	;IF @strDefinition IS NOT NULL
		SET @strQuery	= (
			SELECT IIF(CHARINDEX(@br+N'--≡≡≡*/',S.[item])=0,NULL,
				REPLACE(LEFT(S.[item],CHARINDEX(@br+N'--≡≡≡*/',S.[item])),W.a,''))
			FROM q._split(@strDefinition,N'/*≡≡≡--') S
			,(SELECT a=ISNULL(@strQueryName,@strDefaultName)+N'--≡≡≡'+@br) W
			WHERE s.[item] LIKE W.a+'%'
		)

	-- If Query was not found, try to get it from LocalVariable table
	;IF @strQuery IS NULL
		SET @strQuery	= [SM].[system].[ufn_GetLocalVariable](ISNULL(@strQueryName,@strDefaultName), @strGroupNameOrProcID)
	
	-- If Query was found and user has sent variables
	;IF @strQuery	IS NOT NULL
	AND @xmlVars	IS NOT NULL
		SET @strQuery	= q._input(@strQuery,@xmlVars)

	;RETURN @strQuery
;END
GO
;EXEC q.sys_proc_drop 1.0,'_merge'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-18 20:15
-- Version: 	1.0
-- Description:	Merge two XML data sets
-- =============================================
CREATE FUNCTION q._merge(
	 @xml_vars_1	XML
	,@xml_vars_2	XML
)
RETURNS XML AS BEGIN
	;DECLARE @xml_vars	XML

	;SET @xml_vars	= (
		SELECT * FROM (
			SELECT
				 [@id]			= ROW_NUMBER() OVER (ORDER BY ISNULL(T2.[a], T1.[a]), ISNULL(T2.[id], T1.[id]),ISNULL(T2.[name], T1.[name]))
				,[@name]		= ISNULL(T2.[name],		T1.[name])
				,[@type]		= ISNULL(T2.[type],		T1.[type])
				,[@is_null]		= ISNULL(T2.[is_null],	T1.[is_null])
				,[@datetime]	= ISNULL(T2.[datetime],	T1.[datetime])
				,[text()]		= ISNULL(T2.[value],	T1.[value])
			FROM		(SELECT [a]=1,* FROM q._show(@xml_vars_1)) T1
			FULL JOIN	(SELECT [a]=2,* FROM q._show(@xml_vars_2)) T2 ON T2.[name] = T1.[name]
		) Q
		FOR XML PATH('item'), ROOT('vars')
	)

	;RETURN @xml_vars
;END
GO
;EXEC q.sys_proc_drop 1.0,'_drop'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-18 19:00
-- Version: 	1.0
-- Description:	Delete variable from XML data set
-- =============================================
CREATE FUNCTION q._drop(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS XML AS BEGIN
	;IF @xml_vars IS NOT NULL
		SET @xml_vars.modify('delete(/vars/item[@name=sql:variable("@name")])')

	;RETURN @xml_vars
;END
GO
;EXEC q.sys_proc_drop 1.0,'_add'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._add(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			NVARCHAR(MAX)	= NULL
	,@type			NVARCHAR(255)	= NULL
)
RETURNS XML AS BEGIN
	;DECLARE	@cur_id			INT
	;DECLARE	@is_null		INT
	;DECLARE	@xml_row		XML

	;SET @is_null = IIF(@value IS NULL,1,0)

	;IF @type IS NULL SET @type = 'NVARCHAR'

	;IF @value IS NOT NULL
		SET @type = (
			CASE @type
				WHEN 'INT'				THEN IIF(TRY_CONVERT(INT,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'BIGINT'			THEN IIF(TRY_CONVERT(BIGINT,			@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'FLOAT'			THEN IIF(TRY_CONVERT(FLOAT,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'REAL'				THEN IIF(TRY_CONVERT(REAL,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'NUMERIC'			THEN IIF(TRY_CONVERT(NUMERIC,			@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'MONEY'			THEN IIF(TRY_CONVERT(MONEY,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'DATE'				THEN IIF(TRY_CONVERT(DATE,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'TIME'				THEN IIF(TRY_CONVERT(TIME,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'DATETIME'			THEN IIF(TRY_CONVERT(DATETIME,			@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'XML'				THEN IIF(TRY_CONVERT(XML,				@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'GUID'				THEN IIF(TRY_CONVERT(UNIQUEIDENTIFIER,	@value)		IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'UNIQUEIDENTIFIER'	THEN IIF(TRY_CONVERT(UNIQUEIDENTIFIER,	@value)		IS NOT NULL	,'GUID'	,'NVARCHAR')
				WHEN 'BIN'				THEN IIF(TRY_CONVERT(VARBINARY(MAX),	@value,	1)	IS NOT NULL	,@type	,'NVARCHAR')
				WHEN 'VARBINARY'		THEN IIF(TRY_CONVERT(VARBINARY(MAX),	@value,	1)	IS NOT NULL	,'BIN'	,'NVARCHAR')
				ELSE 'NVARCHAR'
			END
		)

	;IF @xml_vars IS NULL SET @xml_vars = CAST('<vars />' AS XML)

	;SET @cur_id = ISNULL((
			SELECT x.value('@id[1]', 'INT')
			FROM @xml_vars.nodes('/vars/item') a(x)
			WHERE x.value('@name[1]', 'NVARCHAR(255)') = @name
		),0)

	;IF @cur_id > 0
		SET @xml_vars = q._drop(@xml_vars,@name)
	ELSE
		SET @cur_id = 1+ISNULL((
			SELECT MAX(x.value('@id[1]', 'INT'))
			FROM @xml_vars.nodes('/vars/item') a(x)
		),0)

	;SET @xml_row = (
		SELECT
			 [@id]			= @cur_id
			,[@name]		= @name
			,[@type]		= @type
			,[@is_null]		= @is_null
			,[@datetime]	= GETDATE()
			,[text()]		= @value
		FOR XML PATH('item')
	)

	;SET @xml_vars.modify('insert sql:variable("@xml_row") as last into (/vars)[1]')

	;RETURN @xml_vars
;END
GO
;EXEC q.sys_proc_drop 1.0,'_set'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-21 07:50
-- Version: 	1.0
-- Description:	Check variable was set from parent
--				procedure in dynamic SQL
-- =============================================
CREATE FUNCTION q._set(
	 @strVarName		NVARCHAR(255)
	,@strVarValue		NVARCHAR(MAX)
	,@strVarDefault		NVARCHAR(MAX)	= NULL
	,@intNullIsDefault	INT				= 1
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;RETURN IIF(
		 @strVarValue = '["'+@strVarName+'"]'
		,@strVarDefault
		,IIF(@strVarValue='NULL'
			,IIF(@intNullIsDefault=0,NULL,@strVarDefault)
			,@strVarValue
		)
	)
;END
GO
;EXEC q.sys_proc_drop 1.01,'_prepare'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2017-03-02 12:00
-- Update date:	2022-12-23 07:52
-- Version: 	1.01
-- Description:	Query prepare
-- =============================================
CREATE FUNCTION q._prepare(
	 @strQuery	NVARCHAR(MAX)
	,@xmlVars	XML				= NULL
	,@intDebug	INT				= 0
	,@strParent	NVARCHAR(MAX)	= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE

	-- FUNCTION START
		;SET @strQuery	= q.sys_replace(@strQuery, DEFAULT, DEFAULT, DEFAULT)

		;IF @xmlVars IS NOT NULL
			SET @strQuery	= q._input(@strQuery,@xmlVars)

		;SET @strQuery	= q.sys_info(@strQuery, @intDebug, @strParent)
	-- FUNCTION END

	-- RETURN
		;RETURN @strQuery
;END
GO
;EXEC q.sys_proc_drop 1.01,'_getpr'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-22 17:50
-- Update date:	2022-12-23 07:53
-- Version: 	1.01
-- Description:	Get Query from table or definition
--				with preparation
-- =============================================
CREATE FUNCTION q._getpr(
	 @strQueryName			NVARCHAR(255)	= NULL
	,@strGroupNameOrProcID	NVARCHAR(255)	= NULL
	,@xmlVars				XML				= NULL
	,@intDebug				INT				= NULL
	,@strParent				NVARCHAR(MAX)	= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;RETURN q._prepare(q._get(@strQueryName,@strGroupNameOrProcID,@xmlVars),NULL,@intDebug,@strParent)
;END
GO
;EXEC q.sys_proc_drop 1.01,'_xml'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Update date:	2023-01-18 15:00
-- Version: 	1.01
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._xml(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			XML				= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,CAST(@value AS NVARCHAR(MAX)),'XML')
;END
GO
;EXEC q.sys_proc_drop 1.01,'_bin'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Update date:	2023-01-18 15:00
-- Version: 	1.01
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._bin(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			VARBINARY(MAX)	= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,CONVERT(NVARCHAR(MAX),@value,1),'BIN')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_float'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._float(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			FLOAT			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'FLOAT')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_int'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._int(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			INT				= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'INT')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_date'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._date(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			DATE			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,CONVERT(CHAR(8),@value,112),'DATE')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_time'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._time(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			TIME			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'TIME')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_datetime'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._datetime(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			DATETIME		= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,CONVERT(NCHAR(25),@value,126),'DATETIME')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_guid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._guid(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			UNIQUEIDENTIFIER= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'GUID')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_bigint'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._bigint(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			BIGINT			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'BIGINT')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_real'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._real(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			REAL			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'REAL')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_money'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._money(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			MONEY			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'MONEY')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_numeric'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._numeric(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			NUMERIC			= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,'NUMERIC')
;END
GO
;EXEC q.sys_proc_drop 1.0,'_str'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-15 20:21
-- Version: 	1.0
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._str(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
	,@value			NVARCHAR(MAX)	= NULL
)
RETURNS XML AS BEGIN
	;RETURN q._add(@xml_vars,@name,@value,DEFAULT)
;END
GO
;EXEC q.sys_proc_drop 1.01,'_obj'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 01:40
-- Update date:	2022-12-26 03:16
-- Version: 	1.01
-- Description:	Add variable in XML data set
-- =============================================
CREATE FUNCTION q._obj(
	 @xml_vars		XML				= NULL
	,@name_db		NVARCHAR(255)
	,@name_schema	NVARCHAR(255)
	,@name_object	NVARCHAR(255)
	,@value			NVARCHAR(MAX)	= NULL
)
RETURNS XML AS BEGIN
	;SELECT @xml_vars = q._str(@xml_vars,IIF([number]=3,@name_db,IIF([number]=2,@name_schema,@name_object)),q.sys_unquote(REVERSE(A.[item])))
	FROM q._split(REVERSE(@value),'.') A
	;RETURN @xml_vars
;END
GO
;EXEC q.sys_proc_drop 1.0,'_bigint_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._bigint_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS BIGINT AS BEGIN
	;RETURN (SELECT [var_bigint] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_bin_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._bin_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS VARBINARY(MAX) AS BEGIN
	;RETURN (SELECT [var_bin] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_date_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._date_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS DATE AS BEGIN
	;RETURN (SELECT [var_date] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_datetime_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._datetime_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS DATETIME AS BEGIN
	;RETURN (SELECT [var_datetime] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_float_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._float_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS FLOAT AS BEGIN
	;RETURN (SELECT [var_float] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_guid_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._guid_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS UNIQUEIDENTIFIER AS BEGIN
	;RETURN (SELECT [var_guid] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_int_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._int_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS INT AS BEGIN
	;RETURN (SELECT [var_int] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_money_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._money_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS MONEY AS BEGIN
	;RETURN (SELECT [var_money] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_numeric_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._numeric_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS NUMERIC AS BEGIN
	;RETURN (SELECT [var_numeric] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_real_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._real_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS REAL AS BEGIN
	;RETURN (SELECT [var_real] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_str_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._str_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS NVARCHAR(MAX) AS BEGIN
	;RETURN (SELECT [var_nvarchar] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_time_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._time_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS TIME AS BEGIN
	;RETURN (SELECT [var_time] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_val_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._val_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS NVARCHAR(MAX) AS BEGIN
	;DECLARE @val NVARCHAR(MAX)
	;SET @val = (SELECT TOP 1 [value] FROM q._show(@xml_vars) S WHERE [name] = @name)

	;DECLARE @intVarID		INT
	;DECLARE @strVarName	NVARCHAR(255)
	;DECLARE @strVarType	NVARCHAR(255)
	;DECLARE @strVarValue	NVARCHAR(MAX)

	;DECLARE cur CURSOR FOR (SELECT [name],[type],[value] FROM q._show(@xml_vars) WHERE [name] <> @name)
	;OPEN cur
	;FETCH NEXT FROM cur INTO @strVarName,@strVarType,@strVarValue
	;WHILE @@FETCH_STATUS = 0 BEGIN
		;SET @val	= q.sys_inject(@val,@strVarName,@strVarValue,@strVarType)

		;FETCH NEXT FROM cur INTO @strVarName,@strVarType,@strVarValue
	;END
	;CLOSE cur
	;DEALLOCATE cur

	;RETURN @val
;END
GO
;EXEC q.sys_proc_drop 1.0,'_xml_'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-12-07 03:15
-- Version: 	1.0
-- Description:	Get variable from XML data set
-- =============================================
CREATE FUNCTION q._xml_(
	 @xml_vars		XML				= NULL
	,@name			NVARCHAR(255)
)
RETURNS XML AS BEGIN
	;RETURN (SELECT [var_xml] FROM q._show(@xml_vars) S WHERE [name] = @name)
;END
GO
;EXEC q.sys_proc_drop 1.0,'_clear'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-25 20:44
-- Version: 	1.0
-- Description:	Crear query from comments
-- =============================================
CREATE FUNCTION q._clear(
	 @strQuery	NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX) AS
BEGIN
	;DECLARE @i		INT	= 0
	;DECLARE @l		INT = LEN(@strQuery)

	;DECLARE @an	INT = ASCII(SUBSTRING(@strQuery,1,1))
	;DECLARE @ap	INT
	;DECLARE @ac	INT

	;DECLARE @a CHAR(1)
	;DECLARE @f CHAR(1) = ''
	;DECLARE @r NVARCHAR(MAX)

	;WHILE @i <= @l BEGIN
		;SET @i += 1
		;SET @a = SUBSTRING(@strQuery,@i,1)
		;SET @ap = @ac
		;SET @ac = @an
		;SET @an = ASCII(SUBSTRING(@strQuery,@i+1,1))
			
		;IF CHAR(@ac)='''' BEGIN
			;SET @ac = @ac
		END

		;IF (@f='"' AND CHAR(@ac)='"'	AND CHAR(@an)<>'"')
		OR	(@f='`' AND CHAR(@ac)=''''	AND CHAR(@an)<>'''')
			SET @f = ''
		ELSE
			IF @f = '' BEGIN
				;SET @f = IIF('--'	= CHAR(@ac)+CHAR(@an)	,'-', @f)
				;SET @f = IIF('/*'	= CHAR(@ac)+CHAR(@an)	,'/', @f)
				;SET @f = IIF('"'	= CHAR(@ac)				,'"', @f)
				;SET @f = IIF('['	= CHAR(@ac)				,'[', @f)
				;SET @f = IIF(''''	= CHAR(@ac)				,'`', @f)
			;END

		;IF @f NOT IN ('-','/')
			SET @r = ISNULL(@r,'')+ISNULL(@a,'')

		;IF (@f = '/' AND '*/'	= CHAR(@ap)+CHAR(@ac))
		OR	(@f = '[' AND ']'	= CHAR(@ac))
		OR	(@f = '-' AND @an IN (13,10))
			SET @f = ''
	;END

	;RETURN @r
;END
GO
;EXEC q.sys_proc_drop 1.0,'_param'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2015-09-12 19:23
-- Version: 	1.0
-- Description:	Get Param value from string
-- =============================================
CREATE FUNCTION q._param(
	 @strParametrsString	NVARCHAR(MAX)
	,@strParamSearch		NVARCHAR(255)
	,@strDelimiter			NVARCHAR(255)	= NULL
	,@strEqual				NVARCHAR(255)	= NULL
	,@strDefault			NVARCHAR(255)	= NULL
	,@emtyIsNull			INT				= 1
)
RETURNS NVARCHAR(255)
WITH EXECUTE AS CALLER
AS BEGIN
	-- DECLARE
		;DECLARE @Result	NVARCHAR(255)
		;DECLARE @intStart	INT
		;DECLARE @intEnd	INT

	-- FUNCTION START
		;SET @strDelimiter	= ISNULL(@strDelimiter,';')
		;SET @strEqual		= ISNULL(@strEqual,'=')
		;SET @intStart		= CHARINDEX(@strParamSearch + @strEqual, @strParametrsString, 1)

		;IF @intStart > 0 BEGIN
			;SET @intStart	= @intStart + LEN(@strParamSearch + @strEqual)
			;SET @intEnd	= CHARINDEX(@strDelimiter, @strParametrsString, @intStart)
			;SET @intEnd	= IIF(@intEnd = 0, LEN(@strParametrsString), @intEnd - @intStart)

			;SET @Result	= SUBSTRING(@strParametrsString, @intStart, @intEnd)
		;END ELSE
			SET @Result = NULL

		;IF	@emtyIsNull	<> 0
		AND	@Result		= ''
			SET @Result = NULL

		;IF	@strDefault	IS NOT NULL
		AND	@Result		IS NULL
			SET @Result	= @strDefault

	-- RETURN
		;RETURN @Result

	-- FUNCTION END
;END
GO
;EXEC q.sys_proc_drop 1.1,'sys_proc_start'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-30 17:42
-- Update date: 2023-01-18 00:20
-- Version:		1.1
-- Description:	Create call stack on proc start
-- =============================================
CREATE PROCEDURE q.sys_proc_start
	 @PROC_NAME_OR_ID		NVARCHAR(255)
	,@PROC_CHLD				XML				= NULL	OUTPUT
	,@RESULT				XML				= NULL	OUTPUT
	,@DEBUG					INT				= NULL	OUTPUT
	,@PARENT				NVARCHAR(MAX)	= NULL	OUTPUT
AS BEGIN
	-- DECLARE
		;DECLARE @PROC_ID		INT
		;DECLARE @GUID 			CHAR(36)
		;DECLARE @PROC_NAME		NVARCHAR(255)

		;DECLARE @reset			INT	= 0
		;DECLARE @current_id	INT
		;DECLARE @proc_flows	XML

	-- PROCEDURE START
		;SET @PROC_NAME = IIF(
				 ISNUMERIC(@PROC_NAME_OR_ID)=1
				,q.sys_proc(@PROC_NAME_OR_ID)
				,@PROC_NAME_OR_ID
			)
		;SET @PROC_ID = IIF(ISNUMERIC(@PROC_NAME_OR_ID)=1,@PROC_NAME_OR_ID,NULL)

		;IF @RESULT IS NULL
			SET @reset = 1
	
		;IF @RESULT IS NOT NULL
			IF @RESULT.value('local-name(/*[1])','NVARCHAR(255)') <> 'stack_frame'
				SET @reset = 1
	
		;IF @reset = 1 BEGIN
			;DECLARE @PARENT_NAME	NVARCHAR(255)

			;SET @GUID			= NEWID()
			;SET @PARENT		= ISNULL(@PARENT,'Execution Start')
			;IF RIGHT(@PARENT,1)=';' SET @PARENT = REVERSE(STUFF(REVERSE(@PARENT),1,1,''))
			;SET @PARENT_NAME	= IIF(
					 CHARINDEX(';',@PARENT)=0
					,@PARENT
					,REVERSE(SUBSTRING(REVERSE(@PARENT),1,CHARINDEX(';',REVERSE(@PARENT))-1))
				)
			;SET @current_id	= (SELECT COUNT(*) FROM q._split(@PARENT,';'))
	
			;SET @RESULT		= (
					SELECT
						 [@id]			= @current_id
						,[@name]		= @PARENT_NAME
						,[@type]		= IIF(@PARENT_NAME='Execution Start','INIT','UNDEFINED')
						,[@flow_guid]	= @GUID
						,[@cur_id]		= @current_id
						,[@debug]		= @DEBUG
						,[@start]		= CONVERT(NVARCHAR(255), GETDATE(), 126)
						,[@end]			= ''
						,[@duration]	= ''
						,[@error]		= 0
						,[@user]		= ORIGINAL_LOGIN()
						,[@host]		= HOST_NAME()
						,(
							SELECT
								 [@stack_guid]	= NEWID()
								,(SELECT @PARENT FOR XML PATH('stack_string'),TYPE)
								,(
									SELECT	 [@id]			= S.[number]
											,[@name]		= S.[item]
											,[@flow_guid]	= IIF(S.[number]=@current_id,@GUID,NULL)
									FROM q._split(@PARENT,';') S
									FOR XML PATH('flow'),ROOT('flows'),TYPE
								)
							FOR XML PATH('stack'),TYPE
						)
						,[children]		= ''
						,[errors]		= ''
					FOR XML PATH('stack_frame'),TYPE
				)
		;END

		;SET @current_id	= ISNULL(@RESULT.value('/*[1]/@cur_id','INT'),0)+1
		;SET @GUID			= NEWID()
	
		;SELECT @proc_flows	= (
				SELECT	x.query('(./flows/*)')
						,(SELECT [@id]=@current_id,[@name]=@PROC_NAME,[@flow_guid]=@GUID FOR XML PATH('flow'),TYPE)
				FROM @RESULT.nodes('/*[1]/stack[1]') a(x)
				FOR XML PATH(''),ROOT('flows'),TYPE
			)
		
		;SET @DEBUG = @DEBUG+IIF(@DEBUG=0,0,100)
		;SET @PARENT = (STUFF((
				SELECT ';'+x.value('@name','NVARCHAR(MAX)')
				FROM @proc_flows.nodes('/flows/*') a(x)
				ORDER BY x.value('@id','INT')
				FOR XML PATH('')
			),1,1,''))

		;SET @PROC_CHLD	= (
				SELECT
					 [@id]			= @current_id
					,[@name]		= @PROC_NAME
					,[@type]		= ISNULL((SELECT [type_desc] FROM sys.objects WITH (NOLOCK) WHERE [object_id] = @PROC_ID),'UNDEFINED')
					,[@flow_guid]	= @GUID
					,[@cur_id]		= @current_id
					,[@debug]		= @DEBUG
					,[@start]		= CONVERT(NVARCHAR(255), GETDATE(), 126)
					,[@end]			= ''
					,[@duration]	= ''
					,[@error]		= 0
					,[@user]		= ORIGINAL_LOGIN()
					,[@host]		= HOST_NAME()
					,[@proc_id]		= @PROC_ID
					,(
						SELECT
							 [@stack_guid]	= NEWID()
							,(SELECT @PARENT FOR XML PATH('stack_string'),TYPE)
							,@proc_flows
						FOR XML PATH('stack'),TYPE
					)
					,[children]		= ''
					,[errors]		= ''
				FOR XML PATH('stack_frame'), TYPE
			)
;END
GO
;EXEC q.sys_proc_drop 1.1,'sys_proc_end'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-30 17:42
-- Update date: 2023-01-18 00:20
-- Version:		1.1
-- Description:	Close call stack on proc end
-- =============================================
CREATE PROCEDURE q.sys_proc_end
	 @RESULT		XML				OUTPUT
	,@PROC_CHLD		XML
	,@PROC_INFO		XML				= NULL
	,@ERROR_NUMBER	INT				= NULL
	,@ERROR_MESSAGE	NVARCHAR(MAX)	= NULL
AS BEGIN
	-- DECLARE
		;DECLARE @current_id	INT
		;DECLARE @with_err		INT
		;DECLARE @ERR			XML
		;DECLARE @dt_start		NVARCHAR(255)
		;DECLARE @dt_end		NVARCHAR(255)	= CONVERT(NVARCHAR(255), GETDATE(), 126)
		;DECLARE @dt_duration	NVARCHAR(255)

	-- PROCEDURE START
		;SELECT	 @dt_start		= ISNULL(x.value('@start','NVARCHAR(255)'),@dt_end)
				,@current_id	= ISNULL(x.value('@cur_id','INT'),0)
		FROM @PROC_CHLD.nodes('/*[1]') a(x)
		
		;SET @dt_duration	= CONVERT(NVARCHAR(255), CAST(@dt_end AS DATETIME)-CAST(@dt_start AS DATETIME), 126)

		;SET @PROC_CHLD.modify('replace value of (/*[1]/@end)[1] with sql:variable("@dt_end")')
		;SET @PROC_CHLD.modify('replace value of (/*[1]/@duration)[1] with sql:variable("@dt_duration")')
		;SET @PROC_CHLD.modify('insert <info/> as last into (/*[1])[1]')
		;SET @PROC_CHLD.modify('insert sql:variable("@PROC_INFO") into (/*[1]/info)[1]')
		
		;IF ISNULL(@ERROR_NUMBER,0)!=0 BEGIN
			-- Set Error flag = 1
			;SET @PROC_CHLD.modify('replace value of (/*[1]/@error)[1] with "1"')

			;SET @ERR = (
					SELECT	 [@err_number]			= @ERROR_NUMBER
							,[@err_message]			= @ERROR_MESSAGE
							,[@err_datetime]		= @dt_end
							,[@err_guid]			= NEWID()
							,[@err_is_inherited]	= 0
							,[@stack_guid]			= @PROC_CHLD.value('/*[1]/stack[1]/@stack_guid'	,'CHAR(36)')
							,[@flow_guid]			= @PROC_CHLD.value('/*[1]/@flow_guid'			,'CHAR(36)')
					FOR XML PATH('error'),TYPE
				)
			
			;SET @PROC_CHLD.modify('insert sql:variable("@ERR") into (/*[1]/errors)[1]')
		;END
		
		-- Set Error flag from child
		;SET @with_err	= @PROC_CHLD.value('/*[1]/@error','INT')
		;IF @with_err = 1
			SET @RESULT.modify('replace value of (/*[1]/@error)[1] with "1"')

		-- Add Errors from child
		;SET @ERR = (
				SELECT x.query('(./errors/*)')
				FROM @PROC_CHLD.nodes('/*[1]') b(x)
				FOR XML PATH(''),TYPE
			)
		-- Set Inherited flag
		;DECLARE @err_cnt INT = @ERR.value('count(/error)', 'int')
		;WHILE @err_cnt>0 BEGIN
			;SET @ERR.modify('replace value of (/*[1]/@err_is_inherited)[sql:variable("@err_cnt")] with "1"')
			;SET @err_cnt -= 1
		;END
		;SET @RESULT.modify('insert sql:variable("@ERR") into (/*[1]/errors)[1]')

		-- Set Exec duration
		;SET @dt_start		= ISNULL(@RESULT.value('/*[1]/@start','NVARCHAR(255)'),@dt_end)
		;SET @dt_duration	= CONVERT(NVARCHAR(255), CAST(@dt_end AS DATETIME)-CAST(@dt_start AS DATETIME), 126)

		;SET @RESULT.modify('replace value of (/*[1]/@end)[1] with sql:variable("@dt_end")')
		;SET @RESULT.modify('replace value of (/*[1]/@duration)[1] with sql:variable("@dt_duration")')
		;SET @RESULT.modify('replace value of (/*[1]/@cur_id)[1] with sql:variable("@current_id")')
		;SET @RESULT.modify('insert sql:variable("@PROC_CHLD") into (/*[1]/children)[1]')
;END
GO
;EXEC q.sys_proc_drop 1.0,'sys_proc_err'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-18 00:20
-- Version:		1.0
-- Description:	Get errors from call stack XML-data
-- =============================================
CREATE FUNCTION q.sys_proc_err(
	@RESULT	XML
)
RETURNS TABLE AS RETURN (
	SELECT
		 [err_number]	= b.x.value('@err_number'		,'INT')
		,[err_message]	= b.x.value('@err_message'		,'NVARCHAR(MAX)')
		,[err_datetime]	= b.x.value('@err_datetime'		,'DATETIME')
		,[stack_string]	= d.x.value('./stack[1]/stack_string[1]'	,'NVARCHAR(MAX)')
		,[proc_name]	= d.x.value('@name'				,'SYSNAME')
		,[user_name]	= d.x.value('@user'				,'SYSNAME')
		,[host_name]	= d.x.value('@host'				,'SYSNAME')
		,[proc_id]		= d.x.value('@proc_id'			,'INT')
	FROM		@RESULT.nodes('//node()[error]')		a(x)
	CROSS APPLY	a.x.nodes('./error')					b(x)
	CROSS APPLY @RESULT.nodes('//node()[stack_frame]')	c(x)
	CROSS APPLY c.x.nodes('./stack_frame')				d(x)
	WHERE	b.x.value('./@err_is_inherited','INT')	= 0
		AND b.x.value('./@flow_guid','CHAR(36)')	= d.x.value('./@flow_guid','CHAR(36)')
)
GO
;EXEC q.sys_proc_drop 1.0,'_list'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-22 18:11
-- Version: 	1.0
-- Description:	Show all variables in queries in DB
-- =============================================
CREATE FUNCTION q._list()
RETURNS @t TABLE (
	 [row_num]			INT
	,[query_id]			INT
	,[var_type]			NVARCHAR(255)
	,[var_position]		INT
	,[var_name]			NVARCHAR(255)
	,[name]				NVARCHAR(255)
	,[variable_name]	NVARCHAR(255)
	,[variable_obj]		NVARCHAR(255)
	,[database_name]	NVARCHAR(255)
	,[schema_name]		NVARCHAR(255)
	,[object_name]		NVARCHAR(255)
	,[object_type_desc]	NVARCHAR(255)
) AS BEGIN
	;DECLARE	@i					INT
	;DECLARE	@intVarID			INT
	;DECLARE	@intMaskID			INT
	;DECLARE	@intFirstChar		INT
	;DECLARE	@intLastChar		INT
	;DECLARE	@intVarPosition		INT
	;DECLARE	@intMaskLenBegin	INT
	;DECLARE	@intMaskLenEnd		INT
	;DECLARE	@strNextChar		CHAR(1)
	;DECLARE	@strMaskBegin		NVARCHAR(255)
	;DECLARE	@strMaskEnd			NVARCHAR(255)
	;DECLARE	@strVarType			NVARCHAR(255)
	;DECLARE	@strVarName			NVARCHAR(255)
	;DECLARE	@strQuery			NVARCHAR(MAX)
	;DECLARE	@strSearch			NVARCHAR(MAX)
	;DECLARE	@tblMask			TABLE(
				 [mask_id]			INT IDENTITY(1,1)
				,[mask_begin]		NVARCHAR(255)
				,[mask_end]			NVARCHAR(255)
			)

	;INSERT INTO @tblMask([mask_begin],[mask_end])
	VALUES ('@_',NULL),('@@@',NULL),/*('@@@','@@@'),*/('[#','#]'),('[{','}]'),('[*','*]'),('[!','!]')--,('{','}'),('{#','#}')

	;DECLARE @vars_tblQueries		TABLE(
				 [query_id]			INT IDENTITY(1,1)
				,[query]			NVARCHAR(MAX)
				,[name]				NVARCHAR(MAX)
				,[variable_name]	NVARCHAR(255)
				,[variable_obj]		NVARCHAR(255)
				,[database_name]	NVARCHAR(255)
				,[schema_name]		NVARCHAR(255)
				,[object_name]		NVARCHAR(255)
				,[object_type_desc]	NVARCHAR(255)
			)

	;INSERT INTO @vars_tblQueries([query],[name],[variable_name],[variable_obj],[object_type_desc])
	SELECT [variable_value],'VARS.'+[object_name]+'.'+[variable_name],[variable_name],[object_name],'VARS'
	FROM [SM].[system].[LocalVariables] WITH (NOLOCK)
	ORDER BY [object_name],[variable_name]

	;INSERT INTO @vars_tblQueries([query],[name],[database_name],[schema_name],[object_name],[object_type_desc])
	SELECT [definition],[db_name]+'.'+[schema_name]+'.'+[object_name],[db_name],[schema_name],[object_name],[object_type]
	FROM (
		SELECT
			 [db_name]		= x.value('@db_name',		'SYSNAME')
			,[schema_name]	= x.value('@schema_name',	'SYSNAME')
			,[object_name]	= x.value('@object_name',	'SYSNAME')
			,[object_type]	= x.value('@object_type',	'SYSNAME')
			,[definition]	= x.value('@definition',	'NVARCHAR(MAX)')
		FROM (
			SELECT CAST(([SM].[system].[ufn_getScalarFromQuery](q._prepare(('SELECT CAST((SELECT * FROM ('+CHAR(13)+(
				SELECT STUFF((SELECT CHAR(13)+'UNION ALL '+q._get(NULL,@@PROCID,q._str(NULL,'DBNAME',DB.[name]))
				FROM sys.databases DB
				WHERE	1=1
					AND	DB.[name]	LIKE 'SM%'
					AND	DB.[name]	NOT IN ('SM_Temp','SM_Test')
					AND	DB.[name]	NOT LIKE '%Archive'
				FOR XML PATH(''),TYPE).value('.','NVARCHAR(MAX)'),1,11,'')
			)+CHAR(13)+') Q FOR XML RAW, ROOT(`table`),TYPE) AS NVARCHAR(MAX))'),1,q.sys_proc(@@PROCID),NULL),NULL)) AS XML)
		) a(b) CROSS APPLY b.nodes('/table/row') c(x)
	) Q

	;DECLARE @vars_tblVarsFind		TABLE(
				 [row_num]			INT IDENTITY(1,1)
				,[query_id]			INT
				,[var_type]			NVARCHAR(255)
				,[var_position]		NVARCHAR(255)
				,[var_name]			NVARCHAR(255)
			)

	;DECLARE curVars CURSOR FOR (SELECT [query_id],[query] FROM @vars_tblQueries)
	;OPEN curVars
	;FETCH NEXT FROM curVars INTO @intVarID, @strQuery
	;WHILE @@FETCH_STATUS = 0 BEGIN
		;DECLARE curMasks CURSOR FOR (SELECT [mask_id],[mask_begin],[mask_end] FROM @tblMask)
		;OPEN curMasks
		;FETCH NEXT FROM curMasks INTO @intMaskID, @strMaskBegin, @strMaskEnd
		;WHILE @@FETCH_STATUS = 0 BEGIN
			;SET @intMaskLenBegin	= LEN(@strMaskBegin)
			;SET @intMaskLenEnd		= ISNULL(LEN(@strMaskEnd),0)
			;SET @strVarType		= @strMaskBegin+'VAR'+ISNULL(@strMaskEnd,'')
			;SET @strSearch			= @strQuery+' '
			;SET @intVarPosition	= 0

			;WHILE LEN(@strSearch) > 0 BEGIN
				;SET @intFirstChar		= CHARINDEX(@strMaskBegin, @strSearch, 1)
				;SET @intVarPosition	= ISNULL(@intVarPosition,0)+@intFirstChar

				;IF @intFirstChar = 0
					SET @strSearch = ''
				ELSE BEGIN
					;SET @strSearch		= SUBSTRING(@strSearch,@intFirstChar,LEN(@strSearch))
					;IF LEN(@strSearch) <= @intMaskLenBegin + @intMaskLenEnd
						SET @strSearch = ''
					ELSE BEGIN
						;IF @strMaskEnd IS NULL BEGIN
							;SET @strVarName = ''
						
							;SET @i = 0
							;WHILE 1=1 BEGIN
								;IF LEN(@strSearch) = @intMaskLenBegin+@i BREAK

								;SET @intLastChar	= @intMaskLenBegin+@i
								;SET @strNextChar	= SUBSTRING(@strSearch,@intLastChar+1,1)

								;IF @strNextChar NOT LIKE '[a-z]'
								AND @strNextChar NOT LIKE '[A-Z]'
								AND @strNextChar NOT LIKE '|_' ESCAPE '|'
									BREAK

								;SET @strVarName += @strNextChar
								;SET @i += 1
							;END

							;IF LEN(@strVarName) = 0 SET @strVarName = NULL
						;END ELSE BEGIN
							;SET @intLastChar	= CHARINDEX(@strMaskEnd, @strSearch, @intMaskLenBegin+1)
							;IF @intLastChar = 0
								SET @strVarName	= NULL
							ELSE BEGIN
								;SET @strVarName	= SUBSTRING(@strSearch,@intMaskLenBegin+1,@intLastChar-@intMaskLenBegin-1)

								;SET @i = 0
								;WHILE 1=1 BEGIN
									;IF @i >= LEN(@strVarName) BREAK

									;SET @strNextChar	= SUBSTRING(@strVarName,@i+1,1)

									;IF @strNextChar NOT LIKE '[a-z]'
									AND @strNextChar NOT LIKE '[A-Z]'
									AND @strNextChar NOT LIKE '|_' ESCAPE '|'
									BEGIN
										;SET @strVarName	= NULL
										;BREAK
									END

									;SET @i += 1
								;END
							;END
						;END

						;IF LEN(@strSearch) <= @intMaskLenBegin+@intLastChar 
							SET @strSearch	= ''
						ELSE
							SET @strSearch	= SUBSTRING(@strSearch,@intLastChar+@intMaskLenEnd,LEN(@strSearch))

						;IF @strVarName IS NOT NULL
						INSERT INTO @vars_tblVarsFind([query_id],[var_type],[var_position],[var_name])
						SELECT @intVarID,@strVarType,@intVarPosition,@strVarName
					;END
				;END
			;END

			;FETCH NEXT FROM curMasks INTO @intMaskID, @strMaskBegin, @strMaskEnd
		;END
		;CLOSE curMasks
		;DEALLOCATE curMasks

		;FETCH NEXT FROM curVars INTO @intVarID, @strQuery
	;END
	;CLOSE curVars
	;DEALLOCATE curVars

	;INSERT INTO @t
	SELECT V.*
		,Q.[name]
		,Q.[variable_name]
		,Q.[variable_obj]
		,Q.[database_name]
		,Q.[schema_name]
		,Q.[object_name]
		,Q.[object_type_desc]
		--,query=CAST('<?query --'+Q.query+'--?>' AS XML)
	FROM		@vars_tblVarsFind	V
	INNER JOIN	@vars_tblQueries	Q	ON	Q.[query_id]	= V.[query_id]

	;RETURN
;END
/*≡≡≡--Query--≡≡≡
SELECT
	 [db_name]		= [#DBNAME#]
	,[schema_name]	= s.[name]
	,[object_name]	= o.[name]
	,[object_type]	= O.[type_desc]
	,[definition]	= REPLACE(REPLACE(REPLACE(mo.[definition]
		,s.[name]+'.'+o.[name],QUOTENAME(s.[name])+'.'+QUOTENAME(o.[name]))
		,QUOTENAME(s.[name])+'.'+o.[name],QUOTENAME(s.[name])+'.'+QUOTENAME(o.[name]))
		,s.[name]+'.'+QUOTENAME(o.[name]),QUOTENAME(s.[name])+'.'+QUOTENAME(o.[name]))
FROM		[{DBNAME}].sys.sql_modules	AS mo	WITH (NOLOCK)
INNER JOIN	[{DBNAME}].sys.objects		AS o	WITH (NOLOCK)	ON	o.[object_id]	= mo.[object_id]
INNER JOIN	[{DBNAME}].sys.schemas		AS s	WITH (NOLOCK)	ON	s.[schema_id]	= o.[schema_id]
--≡≡≡*/
GO
;EXEC q.sys_proc_drop 1.0,'help'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2022-11-21 08:26
-- Version: 	1.0
-- Description:	Show example of using Q framework
-- =============================================
CREATE PROCEDURE q.help AS
;DECLARE @strProc	NVARCHAR(255) = q.sys_proc(@@PROCID)
;DECLARE @strQuery	NVARCHAR(MAX) = q._get(DEFAULT,@@PROCID,NULL)
;EXEC [SM].[system].[sp_Exec_Query]
	 @strQuery	= @strQuery
	,@intDebug	= 1
	,@strParent	= @strProc
/*≡≡≡--Query--≡≡≡
-- ==============================================================================================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-18 20:51
-- Description:	Query Builder using instruction and examples
-- ==============================================================================================================
--	[*VAR_NAME*]	fo using in comments (inline or block)
--	[{VAR_NAME}]	for system object names
--						-will be quoted with function QUOTENAME
--	[#VAR_NAME#]	for main using in most cases
--						- var
--						- will be quoted with Single Quotation Mark (') for injection sencitive var types
--						- NULL value and injection insencitive var types will be without Quotation

-- !!! ATENTION !!!
-- !!! Dangerous using without escaping - could be used for code injection
-- !!! This type of var should be set inside the procedure
-- !!! For admin using only (not for external access to var by user)
--		[!VAR_NAME!]	- represented as is
--		["VAR_NAME"]	- represented as is but Single Quotation Mark (') will be escaping and doubled (' -> '')
-- ==============================================================================================================

;USE [SM]

;DECLARE @debug			INT				= q._set('DEBUG'	,'["DEBUG"]'	,0				,DEFAULT)
;DECLARE @parent		NVARCHAR(MAX)	= q._set('PARENT'	,'["PARENT"]'	,'SQL Query;'	,DEFAULT)
;DECLARE @fork			NVARCHAR(255)	= q._set('FORK'		,'["FORK"]'		,'q.help'		,DEFAULT)

;DECLARE @xml_vars		XML
;DECLARE @xml_vars_sys	XML
;DECLARE @br			CHAR(2)=CHAR(13)+CHAR(10)
;DECLARE @print			NVARCHAR(MAX) = ''
;DECLARE @stComment		NVARCHAR(MAX) = ''

;DECLARE @TEMP_TABLE_PATH	NVARCHAR(255)
;DECLARE @DB_NAME			SYSNAME
;DECLARE @SCHEMA_NAME		SYSNAME
;DECLARE @TABLE_NAME		SYSNAME
;DECLARE @STR				NVARCHAR(255)
;DECLARE @INT				INT
;DECLARE @FLT				FLOAT
;DECLARE @XML				XML
;DECLARE @BIN				VARBINARY(MAX)
;DECLARE @GUID				UNIQUEIDENTIFIER
;DECLARE @DATE				DATE
;DECLARE @TIME				TIME
;DECLARE @DATETIME			DATETIME

;IF OBJECT_ID('tempdb..#t1') IS NOT NULL DROP TABLE #t1
;IF OBJECT_ID('tempdb..#t2') IS NOT NULL DROP TABLE #t2

-- =============================================
-- [1] without adding vars
-- =============================================
	;SET @stComment		= '[1] without adding vars'
	;SET @xml_vars_sys	= q._str(@xml_vars,'COMMENT',@stComment)
	;SET @print			+= q._get('EXAMPLES',@fork,q._merge(@xml_vars,@xml_vars_sys))+@br+@br


-- =============================================
-- [2] adding null vars
-- =============================================
	;SET @stComment	= '[2] adding null vars'

	;SET @xml_vars	= q._str(@xml_vars,'TEMP_TABLE_PATH',@TEMP_TABLE_PATH)
	;SET @xml_vars	= q._str(@xml_vars,'DB_NAME',@DB_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'SCHEMA_NAME',@SCHEMA_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'TABLE_NAME',@TABLE_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'STR',@STR)
	;SET @xml_vars	= q._int(@xml_vars,'INT',@INT)
	;SET @xml_vars	= q._xml(@xml_vars,'XML',@XML)
	;SET @xml_vars	= q._bin(@xml_vars,'BIN',@BIN)
	;SET @xml_vars	= q._guid(@xml_vars,'GUID',@GUID)
	;SET @xml_vars	= q._date(@xml_vars,'DATE',@DATE)
	;SET @xml_vars	= q._time(@xml_vars,'TIME',@TIME)
	;SET @xml_vars	= q._float(@xml_vars,'FLT',@FLT)
	;SET @xml_vars	= q._datetime(@xml_vars,'DATETIME',@DATETIME)

	;SET @xml_vars_sys	= q._str(@xml_vars_sys,'COMMENT',@stComment)
	;SET @print			+= q._get('EXAMPLES',@fork,q._merge(@xml_vars,@xml_vars_sys))+@br+@br

	;SELECT [section]=2,[desc]=@stComment,* INTO #t1 FROM q._show(@xml_vars)
	;SELECT [section]=2,[desc]=@stComment,[xml_vars]=@xml_vars INTO #t2


-- =============================================
-- [3] adding not null vars
-- =============================================
	;SET @stComment	= '[3] adding not null vars'

	;SET @TEMP_TABLE_PATH	= '[{DB_NAME}].[{SCHEMA_NAME}].[{TABLE_NAME}]'
	;SET @DB_NAME			= 'SM'
	;SET @SCHEMA_NAME		= 'store'
	;SET @TABLE_NAME		= 'cStore'
	;SET @STR				= 'Your text is here'
	;SET @INT				= 10
	;SET @FLT				= 15.87
	;SET @XML				= (SELECT a=1,b='test',c=2.5 FOR XML RAW, TYPE)
	;SET @BIN				= CONVERT(VARBINARY,NEWID())
	;SET @GUID				= NEWID()
	;SET @DATE				= GETDATE()
	;SET @TIME				= GETDATE()
	;SET @DATETIME			= GETDATE()

	;SET @xml_vars	= q._str(@xml_vars,'TEMP_TABLE_PATH',@TEMP_TABLE_PATH)
	;SET @xml_vars	= q._str(@xml_vars,'DB_NAME',@DB_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'SCHEMA_NAME',@SCHEMA_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'TABLE_NAME',@TABLE_NAME)
	;SET @xml_vars	= q._str(@xml_vars,'STR',@STR)
	;SET @xml_vars	= q._int(@xml_vars,'INT',@INT)
	;SET @xml_vars	= q._xml(@xml_vars,'XML',@XML)
	;SET @xml_vars	= q._bin(@xml_vars,'BIN',@BIN)
	;SET @xml_vars	= q._guid(@xml_vars,'GUID',@GUID)
	;SET @xml_vars	= q._date(@xml_vars,'DATE',@DATE)
	;SET @xml_vars	= q._time(@xml_vars,'TIME',@TIME)
	;SET @xml_vars	= q._float(@xml_vars,'FLT',@FLT)
	;SET @xml_vars	= q._datetime(@xml_vars,'DATETIME',@DATETIME)

	;SET @xml_vars_sys	= q._str(@xml_vars_sys,'COMMENT',@stComment)
	;SET @print			+= q._get('EXAMPLES',@fork,q._merge(@xml_vars,@xml_vars_sys))+@br+@br

	;INSERT INTO #t1 SELECT [section]=3,[desc]=@stComment,* FROM q._show(@xml_vars)
	;INSERT INTO #t2 SELECT [section]=3,[desc]=@stComment,[xml_vars]=@xml_vars


-- =============================================
-- [4] code injection protection
-- =============================================
	;SET @stComment	= '[4] code injection protection'
	
	;SET @xml_vars	= q._drop(q._drop(q._drop(q._drop(q._drop(q._drop(q._drop(q._drop(q._drop(
						@xml_vars,'STR'),'INT'),'FLT'),'XML'),'BIN'),'GUID'),'DATE'),'TIME'),'DATETIME')

	;SET @xml_vars	= q._str(@xml_vars,'INFO_VAR',CHAR(13)+CHAR(9)+CHAR(9)+'Your multiline comment'+CHAR(13)+CHAR(9)+CHAR(9)+'could be here'+CHAR(13)+CHAR(9))
	;SET @xml_vars	= q._str(@xml_vars,'INFO_INJ_1',REPLACE(' #/;EXEC [MY HACK IS HERE];/# ','#','*'))
	;SET @xml_vars	= q._str(@xml_vars,'INFO_INJ_2',CHAR(13)+CHAR(10)+';EXEC [MY HACK IS HERE]; --')

	;SET @xml_vars	= q._str(@xml_vars,'STR_INJ_1','1; EXEC [MY HACK IS HERE]; SELECT 1')
	;SET @xml_vars	= q._str(@xml_vars,'STR_INJ_2','''; EXEC [MY HACK IS HERE]; SELECT ''')

	;SET @xml_vars	= q._str(@xml_vars,'FIELD_NAME','store_id')
	;SET @xml_vars	= q._str(@xml_vars,'FIELD_NAME_INJ','dbo].N(1); EXEC [MY HACK IS HERE]; SELECT [store_id')

	;INSERT INTO #t1 SELECT [section]=4,[desc]=@stComment,* FROM q._show(@xml_vars)
	;INSERT INTO #t2 SELECT [section]=4,[desc]=@stComment,[xml_vars]=@xml_vars


-- =============================================
-- FINISH
-- =============================================
	;SET @stComment	= q._get('MAIN',@fork,@xml_vars)
	;SET @xml_vars	= q._str(NULL,'EXAMPLES',@print)

	;PRINT q._prepare(@stComment,@xml_vars,0,@parent);
	;SELECT * FROM #t1 ORDER BY [section],[id]
	;SELECT * FROM #t2 ORDER BY [section]
--≡≡≡*/
/*≡≡≡--MAIN--≡≡≡
--	[*VAR_NAME*]	fo using in comments (inline or block)
--	[{VAR_NAME}]	for system object names
--						-will be quoted with function QUOTENAME
--	[#VAR_NAME#]	for main using in most cases
--						- var
--						- will be quoted with Single Quotation Mark (') for injection sencitive var types
--						- NULL value and injection insencitive var types will be without Quotation

-- !!! ATENTION !!!
-- !!! Dangerous using without escaping - could be used for code injection
-- !!! This type of var should be set inside the procedure
-- !!! For admin using only (not for external access to var by user)
--		[!VAR_NAME!]	- represented as is
--		["VAR_NAME"]	- represented as is but Single Quotation Mark (') will be escaping and doubled (' -> '')

/*-- Safe Using --*/

	/*-- Get params from Parent --*/
		;DECLARE @debug			INT				= q._set('DEBUG'	,'["DEBUG"]'	,0				,DEFAULT)
		;DECLARE @parent		NVARCHAR(MAX)	= q._set('PARENT'	,'["PARENT"]'	,'SQL Query;'	,DEFAULT)

	/*-- Using COMMENTS --*/
		/*[*INFO*]*/
		-- [*INFO*]

		/*[*INFO_VAR*]*/
		-- [*INFO_VAR*]

	/*-- Using OBJECT NAMES --*/
		;SELECT TOP 1 [{FIELD}]
		FROM [{DB}].[{SCHEMA}].[{TABLE}]

		;SELECT TOP 1 [{FIELD_NAME}]
		FROM [{DB_NAME}].[{SCHEMA_NAME}].[{TABLE_NAME}]

	/*-- Using Variables --*/
		;SELECT TOP 1
			 [INT]		= [#VAR#]
			,[FLT]		= [#VAR#]
			,[STR]		= [#VAR#]
			,[XML]		= [#VAR#]
			,[BIN]		= [#VAR#]
			,[GUID]		= [#VAR#]
			,[DATE]		= [#VAR#]
			,[TIME]		= [#VAR#]
			,[DATETIME]	= [#VAR#]
		FROM [!TABLE_PATH!]

[!EXAMPLES!]

/*-- Unsafe Using (possible SQL-Injections) --*/
	/*-- Injection in COMMENTS --*/
		/*["INFO"]*/
		--[#INFO#]

		/*["INFO_INJ_1"]*/
		--[#INFO_INJ_2#]

	/*-- Injection in STRING --*/
		;SELECT [!VAR!]
		FROM [!TABLE_PATH!]

		;SELECT [!STR_INJ_1!]
		FROM [!TEMP_TABLE_PATH!]

	/*-- Injection in OBJECT NAME --*/
		;SELECT [["FIELD"]]
		FROM [!TABLE_PATH!]

		;SELECT [["FIELD_NAME_INJ"]]
		FROM [!TEMP_TABLE_PATH!]

/*-- Exapmple of Safe Using when trying SQL-Injections) --*/
	/*-- Unsuccessful Injection in COMMENTS --*/
		/*[*INFO*]*/
		--[*INFO*]

		/*[*INFO_INJ_1*]*/
		--[*INFO_INJ_2*]

	/*-- Unsuccessful Injection in STRING (1) --*/
		;SELECT [#VAR#]
		FROM [!TABLE_PATH!]

		;SELECT [#STR_INJ_2#]
		FROM [!TEMP_TABLE_PATH!]

	/*-- Unsuccessful Injection in STRING (2) --*/
		;SELECT '["VAR"]'
		FROM [!TABLE_PATH!]

		;SELECT '["STR_INJ_2"]'
		FROM [!TEMP_TABLE_PATH!]

	/*-- Unsuccessful Injection in OBJECT NAME --*/
		;SELECT [{FIELD}]
		FROM [!TABLE_PATH!]

		;SELECT [{FIELD_NAME_INJ}]
		FROM [!TEMP_TABLE_PATH!]
--≡≡≡*/
/*≡≡≡--EXAMPLES--≡≡≡
		/*-- [*COMMENT*] --*/
			;SELECT TOP 1
				 [INT]		= [#INT#]
				,[FLT]		= [#FLT#]
				,[STR]		= [#STR#]
				,[XML]		= [#XML#]
				,[BIN]		= [#BIN#]
				,[GUID]		= [#GUID#]
				,[DATE]		= [#DATE#]
				,[TIME]		= [#TIME#]
				,[DATETIME]	= [#DATETIME#]
			FROM [!TEMP_TABLE_PATH!]
--≡≡≡*/
GO
;EXEC q.sys_proc_drop 1.0,'help'
GO
;IF OBJECT_ID('q.q') IS NULL CREATE SYNONYM q.q FOR q.help
GO
