-- =============================================
--				Q-Framework
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-15 10:00
-- Update date: 2023-02-03 21:00
-- Version: 	1.31
-- Description:	Framework for working with queries
--				in dynamic SQL
-- =============================================
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 03:00
-- Update date: 2023-02-03 21:00
-- Version: 	1.02
-- Description:	DROP all objects in schema Q
-- =============================================
;DECLARE @RECREATE_ALL INT = 1

;IF @RECREATE_ALL!=0 BEGIN
	;DECLARE @schema		SYSNAME
	;DECLARE @strQuery		NVARCHAR(MAX)
	;DECLARE @strSchemaList	NVARCHAR(255) = 'q,_tmp_dynamic'

	;WHILE LEN(@strSchemaList) > 0 BEGIN
		;IF CHARINDEX(',',@strSchemaList)=0
			SELECT @schema = @strSchemaList,@strSchemaList=''
		ELSE BEGIN
			;SET @schema		= LEFT(@strSchemaList,CHARINDEX(',',@strSchemaList)-1)
			;SET @strSchemaList	= SUBSTRING(@strSchemaList,CHARINDEX(',',@strSchemaList)+1,LEN(@strSchemaList))
		;END

		;IF SCHEMA_ID(@schema) IS NOT NULL BEGIN
			;WHILE EXISTS (SELECT * FROM sys.objects WITH (NOLOCK) WHERE schema_id = SCHEMA_ID(@schema)) BEGIN
				;SELECT TOP 1 @strQuery = REPLACE(REPLACE(
						'DROP ? ['+@schema+'].[#]'
						,'#',[name])
						,'?',(
							 IIF([type]	= 'P'							,'PROCEDURE'
							,IIF([type]	IN ('IF','TF','FT','AF','FN')	,'FUNCTION'
							,IIF([type]	IN ('V')						,'VIEW'
							,IIF([type]	IN ('SN')						,'SYNONYM'
							,IIF([type]	IN ('U')						,'TABLE'
							,NULL)))))
						))
				FROM sys.objects WITH (NOLOCK)
				WHERE	[schema_id]	= SCHEMA_ID(@schema)
					AND	[type]		IN ('IF','TF','FT','AF','FN','P','V','SN','U')
		
				;IF @strQuery IS NOT NULL
					EXEC (@strQuery)
			;END
		
			;WHILE EXISTS (SELECT * FROM sys.xml_schema_collections WITH (NOLOCK) WHERE [schema_id] = SCHEMA_ID(@schema)) BEGIN
				;SELECT TOP 1 @strQuery = REPLACE('DROP XML SCHEMA COLLECTION ['+@schema+'].[#]','#',[name])
				FROM sys.xml_schema_collections WITH (NOLOCK)
				WHERE [schema_id] = SCHEMA_ID(@schema)
		
				;IF @strQuery IS NOT NULL
					EXEC (@strQuery)
			;END
			
			;EXEC('DROP SCHEMA ['+@schema+']')
		;END
	;END
;END
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-02-03 02:25
-- Version: 	1.0
-- Description:	Change DB parameter TRUSTWORTHY
-- =============================================
;DECLARE @s NVARCHAR(MAX) = 'ALTER DATABASE ['+DB_NAME()+'] SET TRUSTWORTHY ON'
;EXEC (@s)
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-01 00:00
-- Update date: 2023-02-02 04:55
-- Version: 	1.01
-- Description:	Create schema Q
-- =============================================
;IF SCHEMA_ID('q')				IS NULL EXEC('CREATE SCHEMA [q]')
;IF SCHEMA_ID('_tmp_dynamic')	IS NULL EXEC('CREATE SCHEMA [_tmp_dynamic]')
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-01 00:00
-- Update date: 2023-02-02 05:45
-- Version: 	1.01
-- Description:	Add permissions for schema Q
-- =============================================
;DECLARE @strRole	SYSNAME = 'public'
;DECLARE @strQuery	NVARCHAR(MAX)

;IF DATABASE_PRINCIPAL_ID(@strRole) IS NOT NULL BEGIN
	;SET @strQuery = 'GRANT SELECT,INSERT,UPDATE,EXECUTE ON SCHEMA::[q] TO ['+@strRole+']'
	;EXEC(@strQuery)
	
	;SET @strQuery = 'GRANT SELECT,INSERT,UPDATE,EXECUTE ON SCHEMA::[_tmp_dynamic] TO ['+@strRole+']'
	;EXEC(@strQuery)
;END
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-14 02:00
-- Version: 	1.0
-- Description:	Drop function sys_ver
-- =============================================
;DECLARE @ver 		FLOAT	= 1.0
;DECLARE @cur_ver	FLOAT	= 0

;DECLARE @strVerString	NVARCHAR(255)	= '-- Version:'
;IF OBJECT_ID('q.sys_ver') IS NOT NULL BEGIN
	;DECLARE @strDefinition	NVARCHAR(MAX) = OBJECT_DEFINITION(OBJECT_ID('q.sys_ver'))
	;DECLARE @start INT = CHARINDEX(@strVerString,@strDefinition)
	;IF @start>0 BEGIN
		;SET @strDefinition = SUBSTRING(@strDefinition,@start+LEN(@strVerString),LEN(@strDefinition))
		;DECLARE @strVersion	NVARCHAR(255) = REPLACE(REPLACE(REPLACE(REPLACE(
			SUBSTRING(@strDefinition,1,CHARINDEX(CHAR(13),@strDefinition))
			,CHAR(9),''),CHAR(10),''),CHAR(13),''),' ','')
		;SET @cur_ver = IIF(ISNUMERIC(@strVersion)=1,CAST(@strVersion AS FLOAT),0)
	;END
	;IF @cur_ver < @ver DROP FUNCTION q.sys_ver
;END
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-14 02:07
-- Version: 	1.0
-- Description:	Get procedure/function current version
-- =============================================
CREATE FUNCTION q.sys_ver(
	 @strProcName	SYSNAME
	,@strSchemaName	SYSNAME	= 'q'
)
RETURNS FLOAT
WITH EXECUTE AS OWNER
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
;IF 1.01 > q.sys_ver('sys_obj_type_name',DEFAULT)
	IF OBJECT_ID('q.sys_obj_type_name') IS NOT NULL
		DROP FUNCTION q.sys_obj_type_name
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 21:00
-- Update date: 2023-01-24 22:30
-- Version: 	1.01
-- Description:	Get object type name
-- =============================================
CREATE FUNCTION q.sys_obj_type_name(
	 @strType	SYSNAME
)
RETURNS SYSNAME
AS BEGIN
	;RETURN  IIF(@strType	IN ('P')						,'PROCEDURE'
			,IIF(@strType	IN ('IF','TF','FT','AF','FN')	,'FUNCTION'
			,IIF(@strType	IN ('V')						,'VIEW'
			,IIF(@strType	IN ('SN')						,'SYNONYM'
			,IIF(@strType	IN ('U')						,'TABLE'
			,NULL)))))
END
GO
;IF 1.04 > q.sys_ver('sys_drop',DEFAULT)
	IF OBJECT_ID('q.sys_drop') IS NOT NULL
		DROP PROCEDURE q.sys_drop
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 20:43
-- Update date: 2023-01-22 01:30
-- Version: 	1.04
-- Description:	Drop procedure or function with
--				lower version
-- =============================================
CREATE PROCEDURE q.sys_drop
	 @cur_version	FLOAT
	,@proc_name		SYSNAME
AS BEGIN
	;SET NOCOUNT ON
	;IF @cur_version > q.sys_ver(@proc_name,DEFAULT) BEGIN
		;IF OBJECT_ID('q.'+@proc_name) IS NOT NULL BEGIN
			;DECLARE @strQuery	NVARCHAR(MAX) = 'DROP ? [q].['+@proc_name+']'
			;SET @strQuery	= REPLACE(@strQuery,'?',(
				SELECT	 q.sys_obj_type_name([type])
				FROM sys.objects WITH (NOLOCK)
				WHERE	[schema_id]	= SCHEMA_ID('q')
					AND	[name]		= @proc_name
			))
			;IF @strQuery IS NOT NULL
				EXEC (@strQuery)
		;END
	;END
END
GO
;EXEC q.sys_drop 1.0,'sys_upd_tbl_ver'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-25 06:30
-- Version:		1.0
-- Description:	Recreate table with new version
-- =============================================
CREATE PROCEDURE q.sys_upd_tbl_ver(
	 @name	SYSNAME	
	,@ver	FLOAT
	,@cols	NVARCHAR(MAX)
)
AS BEGIN
	;DECLARE @strQuery	NVARCHAR(MAX)
	;DECLARE @br		CHAR(2)	= CHAR(13)+CHAR(10)
	;DECLARE @name_curr	SYSNAME	= (
			SELECT [name]
			FROM sys.objects WITH (NOLOCK)
			WHERE	[name]		LIKE @name+'_v%'
				AND	[schema_id]	= SCHEMA_ID('q')
		)
	;DECLARE @ver_curr	FLOAT	= ISNULL(REPLACE(@name_curr,@name+'_v',''),0)
	
	;IF @ver_curr<@ver BEGIN
		-- Drop table
		;EXEC q.sys_drop 1.0,@name_curr

		-- Create new table
		;SET @strQuery = CONCAT(';CREATE TABLE q.[',@name,'_v',@ver,'](',@br,@cols,@br,')')
		;EXEC (@strQuery)
	
		-- Drop synonym
		;EXEC q.sys_drop 1.0,@name
		
		-- Create new synonym
		;SET @strQuery = CONCAT(';CREATE SYNONYM q.[',@name,'] FOR q.[',@name,'_v',@ver,']')
		;EXEC (@strQuery)
	;END
END
GO
;EXEC q.sys_drop 1.0,'sys_def'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-02-03 16:10
-- Version: 	1.0
-- Description:	Return object definition
-- =============================================
CREATE FUNCTION q.sys_def(
	 @intProcID	INT
)
RETURNS NVARCHAR(MAX)
WITH EXECUTE AS OWNER AS BEGIN
	RETURN OBJECT_DEFINITION(@intProcID)
END
GO
;EXEC q.sys_drop 1.0,'x_name'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-06 20:43
-- Version: 	1.0
-- Description:	Return procedure full path
-- =============================================
CREATE FUNCTION q.x_name(
	 @intProcID	INT
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	RETURN ISNULL(DB_NAME()+'.'+OBJECT_SCHEMA_NAME(@intProcID)+'.'+OBJECT_NAME(@intProcID),CONCAT('Undefined Process ID=',@intProcID))
END
GO
;EXEC q.sys_drop 1.0,'sys_vguid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-19 19:10
-- Version:		1.00
-- Description:	Get GUID for using in functions
-- =============================================
CREATE VIEW q.sys_vguid AS
SELECT [guid] = NEWID()
GO
;EXEC q.sys_drop 1.0,'guid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-19 19:10
-- Version:		1.0
-- Description:	Get NEW GUID
-- =============================================
CREATE FUNCTION q.guid()
RETURNS UNIQUEIDENTIFIER
AS BEGIN
	;RETURN (SELECT [guid] FROM q.sys_vguid)
END
GO
;EXEC q.sys_drop 1.0,'conv_int_bin'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-23 20:35
-- Version:		1.0
-- Description:	Convert Integer to Binary string
-- =============================================
CREATE FUNCTION q.conv_int_bin(
	@val BIGINT
)
RETURNS VARCHAR(MAX)
AS BEGIN
	;DECLARE @r VARCHAR(MAX) = ''
	;WHILE @val > 0
		SELECT	 @r		= CAST(@val % 2 AS VARCHAR) + @r
				,@val	= @val / 2
	;RETURN '00x'+@r
END
GO
;EXEC q.sys_drop 1.0,'conv_bin_int'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-23 20:40
-- Version:		1.0
-- Description:	Convert Binary string to Integer
-- =============================================
CREATE FUNCTION q.conv_bin_int(
	@val VARCHAR(MAX)
)
RETURNS BIGINT
AS BEGIN
	;DECLARE @r BIGINT
	;IF LEFT(@val,3) = '00x' BEGIN
		;SELECT @r = 0,@val=RIGHT(@val,LEN(@val)-3)
		;WHILE LEN(@val) > 0 
			SELECT	 @r		= @r * 2 + CAST(LEFT(@val,1) AS INT)
					,@val	= RIGHT(@val,LEN(@val)-1)
	;END
	;RETURN @r
END
GO
;EXEC q.sys_drop 1.01,'conv_win1251_utf8'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2016-03-31 19:00
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Convert string with Win1251 charset
--				to string with UTF-8 charset
-- =============================================
CREATE FUNCTION q.conv_win1251_utf8(
	@s NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;DECLARE @c INT
		;DECLARE @t NVARCHAR(MAX) = ''

	-- FUNCTION START
		;WHILE LEN(@s) > 0 BEGIN
			;SET @c = ASCII(SUBSTRING(@s,1,1))
	
			;SET @t +=	 IIF(NOT @c BETWEEN 0x80 AND 0xFF,	CHAR(@c)
						,IIF(@c >= 0xF0,		CHAR(0xD1) + CHAR(@c-0x70)
						,IIF(@c >= 0xC0,		CHAR(0xD0) + CHAR(@c-0x30)
						,(CASE @c
							WHEN 0xA8	THEN	CHAR(0xD0) + CHAR(0x81) -- Ё
							WHEN 0xB8	THEN	CHAR(0xD1) + CHAR(0x91) -- ё
							-- украинские символы
							WHEN 0xA1	THEN	CHAR(0xD0) + CHAR(0x8E) -- Ў (У)
							WHEN 0xA2	THEN	CHAR(0xD1) + CHAR(0x9E) -- ў (у)
							WHEN 0xAA	THEN	CHAR(0xD0) + CHAR(0x84) -- // Є (Э)
							WHEN 0xAF	THEN	CHAR(0xD0) + CHAR(0x87) -- // Ї (I..)
							WHEN 0xB2	THEN	CHAR(0xD0) + CHAR(0x86) -- // I (I)
							WHEN 0xB3	THEN	CHAR(0xD1) + CHAR(0x96) -- // i (i)
							WHEN 0xBA	THEN	CHAR(0xD1) + CHAR(0x94) -- // є (э)
							WHEN 0xBF	THEN	CHAR(0xD1) + CHAR(0x97) -- // ї (i..)
							-- чувашские символы
							WHEN 0x8C	THEN	CHAR(0xD3) + CHAR(0x90) -- // &#1232; (A)
							WHEN 0x8D	THEN	CHAR(0xD3) + CHAR(0x96) -- // &#1238; (E)
							WHEN 0x8E	THEN	CHAR(0xD2) + CHAR(0xAA) -- // &#1194; (С)
							WHEN 0x8F	THEN	CHAR(0xD3) + CHAR(0xB2) -- // &#1266; (У)
							WHEN 0x9C	THEN	CHAR(0xD3) + CHAR(0x91) -- // &#1233; (а)
							WHEN 0x9D	THEN	CHAR(0xD3) + CHAR(0x97) -- // &#1239; (е)
							WHEN 0x9E	THEN	CHAR(0xD2) + CHAR(0xAB) -- // &#1195; (с)
							WHEN 0x9F	THEN	CHAR(0xD3) + CHAR(0xB3) -- // &#1267; (у)
							-- chars
							WHEN 0xB9	THEN	CHAR(0xE2) + CHAR(0x84) + CHAR(0x96) -- // № (No)
							ELSE '?'
						END)
					)))
			;SELECT @s = SUBSTRING(@s,2,LEN(@s)-1)
		;END

	-- RETURN
 		;RETURN @t
;END
GO
;EXEC q.sys_drop 1.01,'conv_utf8_win1251'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2016-03-31 19:00
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Convert string with UTF-8 charset
--				to string with Win1251 charset
-- =============================================
CREATE FUNCTION q.conv_utf8_win1251(
	@strWhatToConvert NVARCHAR(MAX)
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	-- DECLARE
		;BEGIN
			;DECLARE @intCharPos	INT
			;DECLARE @intCharASCII	INT
			;DECLARE @bolByteShift	INT
			;DECLARE @c1			INT
			;DECLARE @new_c1		INT
			;DECLARE @intNewCharPos	INT
			;DECLARE @new_c2		INT
			;DECLARE @out_i			INT
			;DECLARE @a				INT
			;DECLARE @strOutValue	NVARCHAR(MAX)
		;END
		
	-- FUNCTION START
		;IF @strWhatToConvert IS NOT NULL BEGIN
			;SET @intCharPos	= 1
			;SET @bolByteShift	= 0
			;SET @strOutValue	= ''
			
			;WHILE (@intCharPos <= LEN(@strWhatToConvert)) BEGIN
				;SET @intCharASCII = ASCII(SUBSTRING(@strWhatToConvert, @intCharPos, 1))
				
				-- Standard ASCII (American Standard Code for Information Interchange) character codes <= 127
				;IF (@intCharASCII <= 127)
					SET @strOutValue = @strOutValue + SUBSTRING(@strWhatToConvert,@intCharPos,1)
				
				;IF (@bolByteShift > 0) BEGIN
					;SET @new_c2 = (@c1 & 3) * 64 + (@intCharASCII & 63)
					
				--Right shift @new_c1 2 bits
					;SET @new_c1		= CAST(@c1/2 AS INT)
					;SET @new_c1		= (CAST(@new_c1/2 AS INT)) & 5
					;SET @intNewCharPos	= @new_c1 * 256 + @new_c2
					
					;IF @intNewCharPos = 1025
						SET @out_i = 168
					
					;IF @intNewCharPos = 1105
						SET @out_i = 184
					
					;IF @intNewCharPos <> 1025
					AND @intNewCharPos <> 1105
						SET @out_i = @intNewCharPos - 848
					
					;SET @strOutValue = @strOutValue + char(@out_i)
					;SET @bolByteShift = 0
				;END
				
			-- If ASCII code >= 192
			--Right shift @intCharASCII 5 bits
				;SET @a = CAST(@intCharASCII/2 AS INT)
				;SET @a = CAST(@a/2 AS INT)
				;SET @a = CAST(@a/2 AS INT)
				;SET @a = CAST(@a/2 AS INT)
				;SET @a = CAST(@a/2 AS INT)
				
			-- Reset Bytes
				;IF (@a = 6) BEGIN
					;SET @c1			= @intCharASCII
					;SET @bolByteShift	= 1
				;END
				
				;SET @intCharPos += 1
			;END
		;END
	
	-- RUTURN
		;RETURN @strOutValue
;END
GO
;EXEC q.sys_drop 1.01,'conv_base64_binary'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2015-04-17 17:41
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Convert Base64 string to Binary
-- =============================================
CREATE FUNCTION q.conv_base64_binary(
	  @strValue		VARCHAR(MAX)
)
RETURNS VARCHAR(MAX)
AS BEGIN
	;RETURN CONVERT(VARCHAR(MAX), CAST('' AS XML).value('xs:base64Binary(sql:variable("@strValue"))', 'VARBINARY(MAX)'), 1)
END
GO
;EXEC q.sys_drop 1.01,'conv_binary_base64'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2015-04-17 17:41
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Convert Binary to Base64 string
-- =============================================
CREATE FUNCTION q.conv_binary_base64(
	@strValue	VARCHAR(MAX)
)
RETURNS VARCHAR(MAX)
AS BEGIN
	;DECLARE @binValue	VARBINARY(MAX) = CONVERT(VARBINARY(MAX), @strValue, 1)
	;RETURN CAST('' AS XML).value('xs:base64Binary(sql:variable("@binValue"))', 'VARCHAR(MAX)')
END
GO
;EXEC q.sys_drop 1.01,'conv_date_str'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2011-03-14 11:15:39.290
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Convert Date to String by mask
-- =============================================
CREATE FUNCTION q.conv_date_str(
	 @datetime		DATETIME
	,@format_mask	NVARCHAR(255)
)
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
			REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
			REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
			REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
			REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
			REPLACE(REPLACE(
				@format_mask	COLLATE Latin1_General_CS_AS
				,'mm'		,'mi')
				,'mm'		,'mi')
				,N'ГГГГ'	,'YYYY')
				,N'ГГ'		,'YY')
				,N'ММ'		,'MM')
				,N'ДД'		,'DD')
				,N'чч'		,'hh')
				,N'час'		,'hh')
				,N'мм'		,'mi')
				,N'мин'		,'mi')
				,N'сс'		,'ss')
				,N'сек'		,'ss')
				,N'мс'		,'ms')
				,'YYYY'		,				DATENAME(YY, @datetime))
				,'YY'		,RIGHT(			DATENAME(YY, @datetime),2))
				,'Month'	,				DATENAME(MM, @datetime))
				,'MON'		,UPPER(LEFT(	DATENAME(MM, @datetime),3)))
				,'Mon'		,	   LEFT(	DATENAME(MM, @datetime),3))
				,'MM'		,RIGHT('0'+		DATEPART(MM, @datetime),2))
				,'DD'		,RIGHT('0'+		DATENAME(DD, @datetime),2))
				,'hh'		,RIGHT('0'+		DATENAME(HH, @datetime),2))
				,'mi'		,RIGHT('0'+		DATENAME(MI, @datetime),2))
				,'ss'		,RIGHT('0'+		DATENAME(SS, @datetime),2))
				,'ms'		,				DATENAME(MS, @datetime))
				,'M'		,				DATEPART(MM, @datetime))
				,'D'		,				DATEPART(DD, @datetime))
				,'s'		,				DATEPART(SS, @datetime))
END
GO
;EXEC q.sys_drop 1.01,'conv_date_unixtime'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2016-02-09 11:22
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Concert Date to Unix Datetime
-- =============================================
CREATE FUNCTION q.conv_date_unixtime(
	@dtDate	DATETIME
)
RETURNS BIGINT 
AS BEGIN
    ;RETURN DATEDIFF(SECOND,GETDATE(),GETUTCDATE()) + IIF(
			@dtDate >= '20380119'
			,CONVERT(BIGINT, DATEDIFF(S, '19700101', '20380119')) 
			+CONVERT(BIGINT, DATEDIFF(S, '20380119', @dtDate)) 
			,DATEDIFF(S, '19700101', @dtDate)
		)
;END
GO
;EXEC q.sys_drop 1.01,'conv_unixtime_date'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2016-02-09 11:22
-- Update date: 2023-02-03 20:00
-- Version:		1.01
-- Description:	Concert Unix Datetime to Date
-- =============================================
CREATE FUNCTION q.conv_unixtime_date(
	@intUnixTime	BIGINT
)
RETURNS DATETIME
AS BEGIN
    ;RETURN DATEADD(SECOND,@intUnixTime - DATEDIFF(SECOND,GETDATE(),GETUTCDATE()), CAST('1970-01-01 00:00:00' AS DATETIME))
;END
GO
;EXEC q.sys_drop 1.0,'_split'
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
;EXEC q.sys_drop 1.0,'_split_row'
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
;EXEC q.sys_drop 1.0,'sys_safe'
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
;EXEC q.sys_drop 1.0,'sys_quote'
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
;EXEC q.sys_drop 1.0,'sys_unquote'
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
;EXEC q.sys_drop 1.0,'sys_inject'
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
;EXEC q.sys_drop 1.0,'_show'
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
;EXEC q.sys_drop 1.0,'_input'
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
;EXEC q.sys_drop 1.04,'_get'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-11-21 07:48
-- Update date: 2023-02-03 21:00
-- Version: 	1.04
-- Description:	Get Query from object definition
-- =============================================
CREATE FUNCTION q._get(
	 @strQueryName			NVARCHAR(255)	= NULL
	,@strGroupNameOrProcID	NVARCHAR(255)	= NULL
	,@xmlVars				XML				= NULL
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;RETURN q._input((
		SELECT IIF(CHARINDEX(CHAR(13)+CHAR(10)+N'--≡≡≡*/',S.[item])=0,NULL,
			REPLACE(LEFT(S.[item],CHARINDEX(CHAR(13)+CHAR(10)+N'--≡≡≡*/',S.[item])),W.a,''))
		FROM q._split(q.sys_def(IIF(
			 ISNUMERIC(@strGroupNameOrProcID)=1
			,@strGroupNameOrProcID				-- User sent ID of the Object
			,OBJECT_ID(@strGroupNameOrProcID)	-- User sent NAME of the Object
		)),N'/*≡≡≡--') S
		,(SELECT a=ISNULL(@strQueryName,'Query')+N'--≡≡≡'+CHAR(13)+CHAR(10)) W
		WHERE s.[item] LIKE W.a+'%'
	),@xmlVars)
;END
GO
;EXEC q.sys_drop 1.0,'_merge'
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
;EXEC q.sys_drop 1.0,'_drop'
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
;EXEC q.sys_drop 1.0,'_add'
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
;EXEC q.sys_drop 1.0,'_set'
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
;EXEC q.sys_drop 1.01,'_xml'
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
;EXEC q.sys_drop 1.01,'_bin'
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
;EXEC q.sys_drop 1.0,'_float'
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
;EXEC q.sys_drop 1.0,'_int'
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
;EXEC q.sys_drop 1.0,'_date'
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
;EXEC q.sys_drop 1.0,'_time'
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
;EXEC q.sys_drop 1.0,'_datetime'
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
;EXEC q.sys_drop 1.0,'_guid'
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
;EXEC q.sys_drop 1.0,'_bigint'
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
;EXEC q.sys_drop 1.0,'_real'
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
;EXEC q.sys_drop 1.0,'_money'
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
;EXEC q.sys_drop 1.0,'_numeric'
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
;EXEC q.sys_drop 1.0,'_str'
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
;EXEC q.sys_drop 1.01,'_obj'
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
;EXEC q.sys_drop 1.0,'_bigint_'
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
;EXEC q.sys_drop 1.0,'_bin_'
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
;EXEC q.sys_drop 1.0,'_date_'
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
;EXEC q.sys_drop 1.0,'_datetime_'
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
;EXEC q.sys_drop 1.0,'_float_'
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
;EXEC q.sys_drop 1.0,'_guid_'
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
;EXEC q.sys_drop 1.0,'_int_'
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
;EXEC q.sys_drop 1.0,'_money_'
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
;EXEC q.sys_drop 1.0,'_numeric_'
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
;EXEC q.sys_drop 1.0,'_real_'
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
;EXEC q.sys_drop 1.0,'_str_'
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
;EXEC q.sys_drop 1.0,'_time_'
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
;EXEC q.sys_drop 1.0,'_val_'
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
;EXEC q.sys_drop 1.0,'_xml_'
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
;EXEC q.sys_drop 1.0,'_clear'
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
;EXEC q.sys_drop 1.0,'_param'
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
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2023-01-24 22:30
-- Update date:	2023-01-31 21:21
-- Version: 	1.12
-- Description:	Table with user queries
-- =============================================
;DECLARE @cols	NVARCHAR(MAX) =
'	 [query_id]		INT					NOT NULL	IDENTITY(1,1)
	,[db_name]		SYSNAME				NOT NULL	DEFAULT(DB_NAME())
	,[user_name]	SYSNAME				NOT NULL	DEFAULT(ORIGINAL_LOGIN())
	,[host]			SYSNAME				NOT NULL	DEFAULT(HOST_NAME())
	,[session_id]	INT					NOT NULL
	,[session_guid]	UNIQUEIDENTIFIER	NOT NULL
	,[flow_guid]	UNIQUEIDENTIFIER	NOT NULL
	,[stack_string]	NVARCHAR(MAX)		NOT NULL
	,[start]		DATETIME			NOT NULL	DEFAULT(GETDATE())
	,[end]			DATETIME			NULL
	,[duration]		DATETIME			NULL
	,[query]		NVARCHAR(MAX)		NOT NULL
	,CONSTRAINT [PK_tlog_queries] PRIMARY KEY CLUSTERED (
		 [query_id]	ASC
		,[db_name]	ASC
	)'

;EXEC q.sys_upd_tbl_ver 'tlog_queries',1.12,@cols
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2023-01-25 06:30
-- Update date:	2023-01-31 21:21
-- Version: 	1.01
-- Description:	Table with errors
-- =============================================
;DECLARE @cols	NVARCHAR(MAX) =
'	 [error_id]		INT					NOT NULL	IDENTITY(1,1)
	,[db_name]		SYSNAME				NOT NULL	DEFAULT(DB_NAME())
	,[user_name]	SYSNAME				NOT NULL	DEFAULT(ORIGINAL_LOGIN())
	,[host]			SYSNAME				NOT NULL	DEFAULT(HOST_NAME())
	,[session_id]	INT					NOT NULL
	,[session_guid]	UNIQUEIDENTIFIER	NOT NULL
	,[flow_guid]	UNIQUEIDENTIFIER	NOT NULL
	,[stack_string]	NVARCHAR(MAX)		NOT NULL
	,[err_number]	INT					NOT NULL
	,[err_message]	NVARCHAR(MAX)		NOT NULL
	,[err_line]		INT					NULL
	,[err_severity]	INT					NULL
	,[err_state]	INT					NULL
	,[err_procedure]SYSNAME				NULL
	,[err_datetime]	DATETIME			NOT NULL	DEFAULT(GETDATE())
	,[err_guid]		UNIQUEIDENTIFIER	NOT NULL
	,CONSTRAINT [PK_tlog_errors] PRIMARY KEY CLUSTERED (
		 [error_id]	ASC
		,[db_name]	ASC
	)'

;EXEC q.sys_upd_tbl_ver 'tlog_errors',1.01,@cols
GO
;EXEC q.sys_drop 1.01,'xflags_err'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2023-01-30 04:30
-- Update date:	2023-01-30 21:50
-- Version: 	1.01
-- Description:	View with error flags
-- =============================================
CREATE VIEW q.xflags_err AS 
SELECT
	 [err_flag_throw]	= 1
	,[err_flag_print]	= 2
	,[err_flag_table]	= 4
	,[err_flag_select]	= 8
	,[err_flag_email]	= 16
GO
;EXEC q.sys_drop 1.0,'xflags_log'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2023-01-30 08:30
-- Version: 	1.0
-- Description:	View with log flags
-- =============================================
CREATE VIEW q.xflags_log AS 
SELECT
	 [log_flag_exec]	= 1
	,[log_flag_print]	= 2
	,[log_flag_table]	= 4
	,[log_flag_xml]		= 8
	,[log_flag_hide]	= 16
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date:	2023-01-19 23:40
-- Update date:	2023-01-25 05:45
-- Version: 	1.14
-- Description:	Call stack XML-data schema
-- =============================================
;DECLARE @create	BIT		= 1
;DECLARE @ver		FLOAT	= 1.14
;DECLARE @ver_cur	FLOAT

;IF EXISTS (
	SELECT * FROM sys.xml_schema_collections
	WHERE	[name]		= 'xsX'
		AND	[schema_id]	= SCHEMA_ID('q')
) BEGIN
	;SET @ver_cur = ISNULL((
		SELECT xml_schema_namespace(N'q',N'xsX').query(
		'/xs:schema/xs:complexType[@name="stack_task"]/xs:complexContent/xs:restriction/xs:attribute[@name="version"]')
		FOR XML PATH(''),ROOT('root'),TYPE
	).value('/*[1]/*[1]/@default','FLOAT'),0)

	;IF ISNULL(@ver_cur,0) >= @ver
		SET @create = 0
	ELSE BEGIN
		;EXEC q.sys_drop 1000.0,'x_xml_validate'
		;DROP XML SCHEMA COLLECTION q.xsX
	;END
		
;END

;IF @create = 1 BEGIN
	;DECLARE @strQuery		NVARCHAR(MAX)
	;DECLARE @xml_schema	NVARCHAR(MAX)

	;SET @strQuery		= 'CREATE XML SCHEMA COLLECTION q.xsX AS'+CHAR(13)+CHAR(10)+'N''["XML"]'''
	;SET @xml_schema	= '<?xml version="1.0" encoding="utf-16"?>
<xs:schema	attributeFormDefault="unqualified"
			elementFormDefault="qualified"
			xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xs:element name="stack_task" type="stack_task" />

	<xs:complexType name="config">
		<xs:attribute name="log"					type="xs:integer"	use="required" />
		<xs:attribute name="err_action"				type="xs:integer"	use="required" />
		<xs:attribute name="err_action_chld"		type="xs:integer"	use="required" />
		<xs:attribute name="err_with_inherited"		type="xs:integer"	use="required" />
		<xs:attribute name="err_where_to_search"	type="xs:integer"	use="required" />
		<xs:attribute name="config_string"			type="xs:string"	use="required" />
	</xs:complexType>

	<xs:complexType name="error">
		<xs:attribute name="err_number"				type="xs:integer"	use="required" />
		<xs:attribute name="err_message"			type="xs:string"	use="required" />
		<xs:attribute name="err_line"				type="xs:integer"	use="optional" />
		<xs:attribute name="err_severity"			type="xs:integer"	use="optional" />
		<xs:attribute name="err_state"				type="xs:integer"	use="optional" />
		<xs:attribute name="err_procedure"			type="xs:string"	use="optional" />
		<xs:attribute name="err_datetime"			type="xs:dateTime"	use="required" />
		<xs:attribute name="err_guid"				type="xs:string"	use="required" />
		<xs:attribute name="err_is_inherited"		type="xs:integer"	use="required" />
		<xs:attribute name="flow_guid"				type="xs:string"	use="required" />
		<xs:attribute name="stack_guid"				type="xs:string"	use="optional" />
	</xs:complexType>

	<xs:complexType name="query">
		<xs:simpleContent>
			<xs:extension base="xs:string">
				<xs:attribute name="start"			type="xs:dateTime"	use="required" />
				<xs:attribute name="end"			type="xs:dateTime"	use="optional" />
				<xs:attribute name="duration"		type="xs:dateTime"	use="optional" />
				<xs:attribute name="is_executed"	type="xs:integer"	use="required" />
			</xs:extension>
		</xs:simpleContent>
	</xs:complexType>

	<xs:complexType name="flow">
		<xs:attribute name="id"						type="xs:integer"	use="required" />
		<xs:attribute name="name"					type="xs:string"	use="required" />
		<xs:attribute name="flow_guid"				type="xs:string"	use="optional" />
	</xs:complexType>

	<xs:complexType name="stack_task">
		<xs:sequence>
			<xs:element name="result_state" minOccurs="0" maxOccurs="1" />
			
			<xs:element name="config" minOccurs="0" maxOccurs="1" type="config" />
			
			<xs:element name="stack" minOccurs="0" maxOccurs="1">
				<xs:complexType>
					<xs:sequence>	
						<xs:element name="stack_string" minOccurs="1" maxOccurs="1" type="xs:string" />
						<xs:element name="flows"		minOccurs="1" maxOccurs="1">
							<xs:complexType>
								<xs:sequence>
									<xs:element name="flow"	minOccurs="1" maxOccurs="unbounded"	type="flow" />
								</xs:sequence>
							</xs:complexType>
						</xs:element>
					</xs:sequence>

					<xs:attribute name="stack_guid"	type="xs:string"	use="required" />
				</xs:complexType>
			</xs:element>

			<xs:element name="subtasks" minOccurs="0" maxOccurs="1">
				<xs:complexType>
					<xs:sequence>
						<xs:element name="stack_task" type="stack_task" minOccurs="0" maxOccurs="unbounded" />
					</xs:sequence>
				</xs:complexType>
			</xs:element>

			<xs:element name="errors" minOccurs="0" maxOccurs="unbounded">
				<xs:complexType>
					<xs:sequence>
						<xs:element name="error" minOccurs="0" maxOccurs="unbounded" type="error" />
					</xs:sequence>
				</xs:complexType>
			</xs:element>
			
			<xs:element name="queries" minOccurs="0" maxOccurs="unbounded">
				<xs:complexType>
					<xs:sequence>
						<xs:element name="query" minOccurs="0" maxOccurs="unbounded" type="query" />
					</xs:sequence>
				</xs:complexType>
			</xs:element>

			<xs:element name="info" minOccurs="0" maxOccurs="unbounded" />
		</xs:sequence>

		<xs:attribute name="id"				type="xs:integer"	use="required" />
		<xs:attribute name="name"			type="xs:string"	use="required" />
		<xs:attribute name="type"			type="xs:string"	use="required" />
		<xs:attribute name="flow_guid"		type="xs:string"	use="required" />
		<xs:attribute name="session_id"		type="xs:integer"	use="required" />
		<xs:attribute name="session_guid"	type="xs:string"	use="required" />
		<xs:attribute name="cur_id"			type="xs:integer"	use="required" />
		<xs:attribute name="start"			type="xs:dateTime"	use="required" />
		<xs:attribute name="end"			type="xs:dateTime"	use="optional" />
		<xs:attribute name="duration"		type="xs:dateTime"	use="optional" />
		<xs:attribute name="error"			type="xs:integer"	use="required" />
		<xs:attribute name="user"			type="xs:string"	use="optional" />
		<xs:attribute name="host"			type="xs:string"	use="optional" />
		<xs:attribute name="proc_id"		type="xs:integer"	use="optional" />
		<xs:attribute name="version"		type="xs:float"		use="optional" default="["VERSION"]" />
	</xs:complexType>
</xs:schema>'
	;SET @strQuery	= q._input(@strQuery,q._float(q._str(NULL
		,'XML'		,@xml_schema)
		,'VERSION'	,@ver)
	)
	
	;BEGIN TRY
		;EXEC (@strQuery)
	END TRY
	BEGIN CATCH
		;SELECT @strQuery
	;END CATCH
;END
GO
;EXEC q.sys_drop 1.01,'x_xml_validate'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-20 00:00
-- Update date:	2023-01-20 01:00
-- Version:		1.01
-- Description:	Validate XML with Schema collection
-- =============================================
CREATE FUNCTION q.x_xml_validate(
	@XML	XML	
)
RETURNS INT
AS BEGIN
	;RETURN IIF(@XML IS NULL,NULL,IIF(TRY_CONVERT(XML(q.xsX),@XML) IS NOT NULL,1,0))
END
GO
;EXEC q.sys_drop 1.0,'x_last_id'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-21 19:21
-- Version:		1.0
-- Description:	Get last stack-frame ID in call stack
-- =============================================
CREATE FUNCTION q.x_last_id(
	@X	XML	
)
RETURNS INT
AS BEGIN
	;RETURN IIF(0=ISNULL(q.x_xml_validate(@X),1)
			,NULL
			,@X.value('/*[1]/@cur_id', 'INT')
		)
END
GO
;EXEC q.sys_drop 1.01,'x_stack_by_id'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-21 19:35
-- Update date: 2023-01-22 02:35
-- Version:		1.01
-- Description:	Get stack-frame by ID
-- =============================================
CREATE FUNCTION q.x_stack_by_id(
	 @X		XML	
	,@ID	INT
)
RETURNS XML
AS BEGIN
	;RETURN IIF(
			 0=ISNULL(q.x_xml_validate(@X),1)
			,NULL
			,IIF(
				 @X.value('/*[1]/@id','INT')=@ID
				,@X
				,(
					SELECT b.x.query('.')
					FROM		@X.nodes('//node()[stack_task]')	a(x)
					CROSS APPLY a.x.nodes('./stack_task')			b(x)
					WHERE b.x.value('./@id','INT') = @ID
				)
			)
		)
END
GO
;EXEC q.sys_drop 1.01,'xget_err_status'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-19 19:15
-- Update date:	2023-01-20 01:00
-- Version:		1.01
-- Description:	Get current err status
-- =============================================
CREATE FUNCTION q.xget_err_status(
	 @X	XML
)
RETURNS INT
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('/*[1]/@error','INT')
;END
GO
;EXEC q.sys_drop 1.0,'x_err_action'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 04:20
-- Version:		1.0
-- Description:	Get current err action
-- =============================================
CREATE FUNCTION q.x_err_action(
	 @X	XML
)
RETURNS INT
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/config[1]/@err_action)[1]','INT')
;END
GO
;EXEC q.sys_drop 1.0,'x_err_action_chld'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 04:20
-- Version:		1.0
-- Description:	Get current err action chld
-- =============================================
CREATE FUNCTION q.x_err_action_chld(
	 @X	XML
)
RETURNS INT
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/config[1]/@err_action_chld)[1]','INT')
;END
GO
;EXEC q.sys_drop 1.0,'xget_log'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 04:20
-- Version:		1.0
-- Description:	Get current log
-- =============================================
CREATE FUNCTION q.xget_log(
	 @X	XML
)
RETURNS INT
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/config[1]/@log)[1]','INT')
;END
GO
;EXEC q.sys_drop 1.0,'xget_debug'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 04:20
-- Version:		1.0
-- Description:	Get current debug
-- =============================================
CREATE FUNCTION q.xget_debug(
	 @X	XML
)
RETURNS INT
AS BEGIN
	;RETURN IIF(CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/config[1]/@log)[1]','INT') & 1 = 1,0,1)
;END
GO
;EXEC q.sys_drop 1.0,'xget_parent'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 09:40
-- Version:		1.0
-- Description:	Get parent (stack string) value
-- =============================================
CREATE FUNCTION q.xget_parent(
	 @X	XML
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/stack[1]/stack_string[1]/text())[1]','NVARCHAR(MAX)')
;END
GO
;EXEC q.sys_drop 1.0,'xget_session_guid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-25 05:50
-- Version:		1.0
-- Description:	Get flow GUID
-- =============================================
CREATE FUNCTION q.xget_session_guid(
	 @X	XML
)
RETURNS UNIQUEIDENTIFIER
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/@session_guid)[1]','UNIQUEIDENTIFIER')
;END
GO
;EXEC q.sys_drop 1.0,'xget_flow_guid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 09:40
-- Version:		1.0
-- Description:	Get flow GUID
-- =============================================
CREATE FUNCTION q.xget_flow_guid(
	 @X	XML
)
RETURNS UNIQUEIDENTIFIER
AS BEGIN
	;RETURN CAST(IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) AS XML).value('(/*[1]/@flow_guid)[1]','UNIQUEIDENTIFIER')
;END
GO
;EXEC q.sys_drop 1.01,'x_config_exec'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_exec()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('log=',[log_flag_exec],';') FROM q.xflags_log)
END
GO
;EXEC q.sys_drop 1.01,'x_config_log_print'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_log_print()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('log=',[log_flag_print],';') FROM q.xflags_log)
END
GO
;EXEC q.sys_drop 1.01,'x_config_log_xml'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_log_xml()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('log=',[log_flag_xml],';') FROM q.xflags_log)
END
GO
;EXEC q.sys_drop 1.01,'x_config_log_table'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_log_table()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('log=',[log_flag_table],';') FROM q.xflags_log)
END
GO
;EXEC q.sys_drop 1.01,'x_config_hide_success'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_hide_success()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('log=',[log_flag_hide],';') FROM q.xflags_log)
END
GO
;EXEC q.sys_drop 1.0,'x_config_err_noaction'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_noaction()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_action=0;'
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_throw'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_throw()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action=',[err_flag_throw],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_print'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_print()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action=',[err_flag_print],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_table'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_table()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action=',[err_flag_table],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_select'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_select()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action=',[err_flag_select],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_email'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_email()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action=',[err_flag_email],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.0,'x_config_err_chld_noaction'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_noaction()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_action_chld=0;'
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_chld_throw'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_throw()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action_chld=',[err_flag_throw],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_chld_print'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_print()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action_chld=',[err_flag_print],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_chld_table'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_table()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action_chld=',[err_flag_table],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_chld_select'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_select()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action_chld=',[err_flag_select],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.01,'x_config_err_chld_email'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Update date: 2023-01-30 08:45
-- Version:		1.01
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_err_chld_email()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN (SELECT CONCAT('err_action_chld=',[err_flag_email],';') FROM q.xflags_err)
END
GO
;EXEC q.sys_drop 1.0,'x_config_inherited_current'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_inherited_current()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_with_inherited=0;'
END
GO
;EXEC q.sys_drop 1.0,'x_config_inherited_all'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_inherited_all()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_with_inherited=1;'
END
GO
;EXEC q.sys_drop 1.0,'x_config_search_all'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_search_all()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_where_to_search=-1;'
END
GO
;EXEC q.sys_drop 1.0,'x_config_search_current'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_search_current()
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN 'err_where_to_search=0;'
END
GO
;EXEC q.sys_drop 1.0,'x_config_search_byid'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-24 18:30
-- Version:		1.0
-- Description:	Return config value
-- =============================================
CREATE FUNCTION q.x_config_search_byid(@id INT)
RETURNS NVARCHAR(255)
AS BEGIN
	;RETURN CONCAT('err_where_to_search=',@id,';')
END
GO
;EXEC q.sys_drop 1.08,'x_err_show'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-18 00:20
-- Update date:	2023-01-30 17:40
-- Version:		1.08
-- Description:	Get errors from call stack XML-data
-- =============================================
CREATE FUNCTION q.x_err_show(
	 @X					XML

	-- Show inherited errors or not:
	-- 0	= Show only error from initial flow
	-- 1	= Get Errors from all stack
	,@WITH_INHERITED	INT	= 0

	-- Where Errors should be taken from
	-- -1		- From all stack
	-- 0		- Current stack node (default)
	-- 1...n	- Stack ID
	,@WHERE_TO_SEARCH	INT	= NULL
)
RETURNS TABLE AS RETURN (
	SELECT
		 [stack_id]			= d.x.value('@id'				,'INT')
		,[err_number]		= b.x.value('@err_number'		,'INT')
		,[err_message]		= b.x.value('@err_message'		,'NVARCHAR(MAX)')
		,[err_line]			= b.x.value('@err_line'			,'INT')
		,[err_severity]		= b.x.value('@err_severity'		,'INT')
		,[err_state]		= b.x.value('@err_state'		,'INT')
		,[err_procedure]	= b.x.value('@err_procedure'	,'SYSNAME')
		,[err_datetime]		= b.x.value('@err_datetime'		,'DATETIME')
		,[err_guid]			= b.x.value('@err_guid'			,'UNIQUEIDENTIFIER')
		,[stack_string]		= d.x.value('./stack[1]/stack_string[1]'	,'NVARCHAR(MAX)')
		,[proc_name]		= d.x.value('@name'				,'SYSNAME')
		,[user_name]		= d.x.value('@user'				,'SYSNAME')
		,[host_name]		= d.x.value('@host'				,'SYSNAME')
		,[proc_id]			= d.x.value('@proc_id'			,'INT')
	FROM		(SELECT IIF(ISNULL(q.x_xml_validate(@X),1)=0,NULL,@X) FOR XML PATH(''),ROOT('root'),TYPE) o(x)
	CROSS APPLY o.x.nodes('//node()[error]')		a(x)
	CROSS APPLY	a.x.nodes('./error')				b(x)
	CROSS APPLY o.x.nodes('//node()[stack_task]')	c(x)
	CROSS APPLY c.x.nodes('./stack_task')			d(x)
	WHERE	b.x.value('./@err_is_inherited','INT')	IN (0,IIF(@WITH_INHERITED!=0,1,0))
		AND b.x.value('./@flow_guid','CHAR(36)')	= d.x.value('./@flow_guid','CHAR(36)')
		AND	d.x.value('@id','NVARCHAR(255)')		LIKE (
				IIF(ISNULL(@WHERE_TO_SEARCH,0)=-1
					,'%'
					,IIF(ISNULL(@WHERE_TO_SEARCH,0)=0
						,o.x.value('/*[1]/*[1]/@id','NVARCHAR(255)')
						,CAST(ISNULL(@WHERE_TO_SEARCH,0) AS NVARCHAR(255))
					)
				)
			)
)
GO
;EXEC q.sys_drop 1.07,'x_err_handler'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-19 19:45
-- Update date: 2023-01-30 22:55
-- Version:		1.07
-- Description:	What to do with errors
-- =============================================
CREATE PROCEDURE q.x_err_handler
	  @X				XML

	-- <enum bit flag>
	--	NULL	= Get data from XML config (default)
	--	0		= Nothing to do
	--	1		= THROW first error
	--	2		= Print all errors
	--	4		= Save error into table
	--	8		= Select all errors
	--	16		= Send email with errors list
	--	32,...[n] = User-defined actions

	-- Show inherited errors or not:
	--	NULL	= Get data from XML config (default)
	--	0		= Show only error from initial flow
	--	1		= Get Errors from all stack
	,@WITH_INHERITED	INT	= NULL

	-- Where Errors should be taken from
	--	NULL	= Get data from XML config (default)
	--	-1		= From all stack
	--	0		= Current stack node
	--	1...n	= Stack ID
	,@WHERE_TO_SEARCH	INT	= NULL

	-- What to do with transaction
	--	0		= Nothing to do (default)
	--	1		= Close transaction
	,@CLOSE_TRANSACTION	INT	= NULL
AS BEGIN
	;SET NOCOUNT ON

	-- DECLARE
		;DECLARE @err_num	INT
		;DECLARE @err_msg	NVARCHAR(MAX)
		;DECLARE @q			NVARCHAR(MAX)
		;DECLARE @colums	NVARCHAR(MAX)
		;DECLARE @ERRS		XML

	-- Error flags
		;DECLARE @err_flag_throw	INT
		;DECLARE @err_flag_print	INT
		;DECLARE @err_flag_table	INT
		;DECLARE @err_flag_select	INT
		;DECLARE @err_flag_email	INT

	-- PROCEDURE START
		;IF ISNULL(@CLOSE_TRANSACTION,0)!=0 BEGIN
			;IF XACT_STATE() = 1	COMMIT TRANSACTION
			;IF XACT_STATE() = -1	ROLLBACK TRANSACTION
		;END

		-- Validate XML
		;IF 0=ISNULL(q.x_xml_validate(@X),1)
			THROW 50000,'Call stack XML-variable is not valid',1
		
		-- Set flags
		;SELECT
			 @err_flag_throw	= [err_flag_throw]
			,@err_flag_print	= [err_flag_print]
			,@err_flag_table	= [err_flag_table]
			,@err_flag_select	= [err_flag_select]
			,@err_flag_email	= [err_flag_email]
		FROM q.xflags_err

		;IF @ERR_ACTION IS NULL
			SET @ERR_ACTION			= @X.value('(/*[1]/config[1]/@err_action)[1]'			,'INT')

		;IF @WITH_INHERITED IS NULL
			SET @WITH_INHERITED		= @X.value('(/*[1]/config[1]/@err_with_inherited)[1]'	,'INT')

		;IF @WHERE_TO_SEARCH IS NULL
			SET @WHERE_TO_SEARCH	= @X.value('(/*[1]/config[1]/@err_where_to_search)[1]'	,'INT')

		;SET @ERRS	= (
				SELECT *
				FROM q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH)
				FOR XML PATH('error'),ROOT('errors'),TYPE
			)

		-- If errors exist
		;IF (@ERRS.value('count(/*[1]/error)', 'INT')>0) BEGIN
			-- Print all errors
			;IF (@ERR_ACTION & @err_flag_print) >0 BEGIN
				;SET @colums	= STUFF((
						SELECT ','+QUOTENAME(c.name)
						FROM		sys.objects	o
						INNER JOIN	sys.columns	c	ON c.object_id	= o.object_id
						WHERE	o.name		= 'x_err_show'
							AND	o.schema_id	= SCHEMA_ID('q')
						ORDER BY c.column_id
						FOR XML PATH('')
					),1,1,'')

				;SET @q = q._get('PRINT',@@PROCID,q._int(q._int(q._xml(q._str(q._str(q._str(NULL
					,'DB_NAME'			,DB_NAME())
					,'COL_LIST_1'		,REPLACE(REPLACE(REPLACE(@colums,'],[',CHAR(9)),'[',''),']',''))
					,'COL_LIST_2'		,REPLACE(@colums,',',',CHAR(9),'))
					,'X'				,@X)
					,'WITH_INHERITED'	,@WITH_INHERITED)
					,'WHERE_TO_SEARCH'	,@WHERE_TO_SEARCH)
				)
				;EXEC(@q)
			;END

			-- Add errors in table
			;IF (@ERR_ACTION & @err_flag_table) >0
				INSERT INTO q.tlog_errors(
					 [db_name],[user_name],[host],[session_id],[session_guid],[flow_guid],[stack_string]
					,[err_number],[err_message],[err_line],[err_severity],[err_state],[err_procedure],[err_datetime],[err_guid]
				)
				SELECT DB_NAME(),ORIGINAL_LOGIN(),HOST_NAME(),@@SPID,q.xget_session_guid(@X),q.xget_flow_guid(@X),q.xget_parent(@X)
				,e.[err_number],e.[err_message],e.[err_line],e.[err_severity],e.[err_state],e.[err_procedure],e.[err_datetime],e.[err_guid]
				FROM q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH) e

			-- Select all errors
			;IF (@ERR_ACTION & @err_flag_select) >0
				SELECT * FROM q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH)

			-- Send email
			;IF (@ERR_ACTION & @err_flag_email) >0 BEGIN
				--/* -- Send e-mail example
				;DECLARE @BODY		NVARCHAR(MAX)
				;DECLARE @SUBJ		NVARCHAR(MAX) = CONCAT('[DB ERROR] Procedure "',@X.value('/*[1]/@name', 'NVARCHAR(255)'),'" was executed with error')
				;SET @colums	= STUFF((
						SELECT ','+QUOTENAME(c.name)
						FROM		sys.objects	o
						INNER JOIN	sys.columns	c	ON c.object_id	= o.object_id
						WHERE	o.name		= 'x_err_show'
							AND	o.schema_id	= SCHEMA_ID('q')
						ORDER BY c.column_id
						FOR XML PATH('')
					),1,1,'')
				
				;SET @q	= q._get('EMAIL_BODY',@@PROCID,q._int(q._int(q._xml(q._str(q._str(q._str(NULL
					,'DB_NAME'			,DB_NAME())
					,'COL_LIST_1'		,'[th]='''+REPLACE(REPLACE(REPLACE(@colums,'],[',''',null,[th]='''),'[',''),']','')+'''')
					,'COL_LIST_2'		,'[td]='+REPLACE(@colums,',',',null,[td]='))
					,'X'				,@X)
					,'WITH_INHERITED'	,@WITH_INHERITED)
					,'WHERE_TO_SEARCH'	,@WHERE_TO_SEARCH)
				)

				;DECLARE @t TABLE([var] NVARCHAR(MAX))
				;INSERT INTO @t EXEC(@q)
				;SET @BODY	= ISNULL(REPLACE(REPLACE(REPLACE(
						(SELECT [var] FROM @t)
						,'<table>','<table style="border: 2px solid black; padding: 10px; background-color: #FFFFE0;">')
						,'<th>','<th style="border: 1px solid black; border-collapse: collapse;">')
						,'<td>','<td style="border: 1px solid black; border-collapse: collapse;">')
					,'')

				;EXEC msdb..sp_send_dbmail
					 @profile_name	= 'TEST'
					,@recipients	= 'Your@Email.Corp'
					,@subject		= @SUBJ
					,@body			= @BODY
					,@body_format	= 'HTML'
				--*/
			;END

			-- User-defined action (i.e. send message in UI)
			;IF @ERR_ACTION % 32 >0 BEGIN
				-- todo
				;IF 1=0 PRINT 'todo'
			;END

			-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			-- This step should be the last one in the procedure
			-- =================================================
			-- THROW first error
			;IF (@ERR_ACTION & @err_flag_throw) >0 BEGIN
				;SELECT TOP 1
					 @err_num	= 50000+[err_number]
					,@err_msg	= [err_message]
				FROM q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH)

				;THROW @err_num,@err_msg,1
			;END
		;END

	-- RETURN
		;RETURN 0

	-- SAVED QUERIES
		;BEGIN	-- >>>>>>>>>>>> SAVED QUERIES >>>>>>>>>>>>
/*≡≡≡--PRINT--≡≡≡
;DECLARE @br				CHAR(2)			= CHAR(13)+CHAR(10)
;DECLARE @COL_LIST_1		NVARCHAR(MAX)	= q._set('COL_LIST_1'		,'["COL_LIST_1"]'		,NULL	,DEFAULT)
;DECLARE @X					XML				= q._set('X'				,'["X"]'				,NULL	,DEFAULT)
;DECLARE @WITH_INHERITED	INT				= q._set('WITH_INHERITED'	,'["WITH_INHERITED"]'	,0		,DEFAULT)
;DECLARE @WHERE_TO_SEARCH	INT				= q._set('WHERE_TO_SEARCH'	,'["WHERE_TO_SEARCH"]'	,NULL	,DEFAULT)
;DECLARE @s					NVARCHAR(MAX)

;SET @s = @COL_LIST_1+@br+REPLICATE('=',100)+@br
;SELECT @s+=CONCAT([!COL_LIST_2!])+@br
FROM [{DB_NAME}].q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH)

;PRINT @s
--≡≡≡*/
;
/*≡≡≡--EMAIL_BODY--≡≡≡
;DECLARE @X					XML	= q._set('X'				,'["X"]'				,NULL	,DEFAULT)
;DECLARE @WITH_INHERITED	INT	= q._set('WITH_INHERITED'	,'["WITH_INHERITED"]'	,0		,DEFAULT)
;DECLARE @WHERE_TO_SEARCH	INT	= q._set('WHERE_TO_SEARCH'	,'["WHERE_TO_SEARCH"]'	,NULL	,DEFAULT)
;DECLARE @TBL		NVARCHAR(MAX)

;SET @TBL = CAST((
	SELECT
		(SELECT [!COL_LIST_1!]
		FOR XML PATH('tr'),ROOT('thead'),TYPE)
		,(SELECT [!COL_LIST_2!]
		FROM [{DB_NAME}].q.x_err_show(@X,@WITH_INHERITED,@WHERE_TO_SEARCH)
		FOR XML PATH('tr'),ROOT('tbody'),TYPE)
	FOR XML PATH(''),ROOT('table')
) AS NVARCHAR(MAX))

;SELECT @TBL
--≡≡≡*/
			;DECLARE @savedqueries INT
		;END	-- <<<<<<<<<<<< SAVED QUERIES <<<<<<<<<<<<
;END
GO
;EXEC q.sys_drop 1.0,'config_string_old'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-25 20:05
-- Version:		1.0
-- Description:	Create config string with Debug support
-- =============================================
CREATE FUNCTION q.config_string_old(
	 @X XML
	,@intDebug	INT
)
RETURNS NVARCHAR(MAX)
AS BEGIN
	;RETURN IIF(@intDebug=0,q.xget_log(@X),2+(q.xget_log(@X)&4)+(q.xget_log(@X)&8)+(q.xget_log(@X)&16)+(q.xget_log(@X)&32))
;END
GO
;EXEC q.sys_drop 1.04,'config'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-23 09:00
-- Update date: 2023-01-30 21:50
-- Version:		1.04
-- Description:	Create config xml
-- =============================================
CREATE FUNCTION q.config(
	-- Combination of all parameters below
	-- <LOG>[,<ERR_ACTION>[,<ERR_ACTION_CHLD>[,<ERR_WITH_INHERITED>[,<ERR_WHERE_TO_SEARCH>]]]]
	--
	-- OR list of parametes like:
	--		log=2;log=8;err_action=2;err_where_to_search=0
	--
	-- Default value: 1,1,1,0,0
	-- Example of using:
	--		1   	= Regular execution with THROW on any error
	--		2   	= Print query in console only
	--		1,0,0	= Exec query and pass error handler for all procedures
	--		7,8,0	= Print the query in the console, write it to XML and execute.
	--				  At the end of the procedure, send an email with any errors that have occurred
	--		,,,-1	= Use default params except the last one - errors will be searching in all stack
	 @CONFIG_STRING	NVARCHAR(MAX) = NULL
	
	-- <LOG>
		-- How to act with dynamic SQL query
		-- <enum bit flag>
		--	1		= Execute query (default)
		--	2		= Print query in console
		--	4		= Write query in a table
		--	8		= Write query in XML
		--	16		= Hide succesfull call stack additional info
		--	32,...[n] = User-defined actions

	-- <ERR_ACTION>
		-- Error action for end of procedure
		-- <enum bit flag>
		--	0		= Nothing to do
		--	1		= THROW first error (default)
		--	2		= Print all errors
		--	4		= Save error into table
		--	8		= Select all errors
		--	16		= Send email with errors list
		--	32,...[n] = User-defined actions
	
	-- <ERR_ACTION_CHLD>
		-- Error action for body of procedure
		-- and subroutines
		-- <enum bit flag> same as in ERR_ACTION
		--	1		= THROW first error (default)

	-- <ERR_WITH_INHERITED>
		-- Show inherited errors or not:
		--	0		= Show only error from initial flow (default)
		--	1		= Get Errors from all stack

	-- <ERR_WHERE_TO_SEARCH>
		-- Where Errors should be taken from
		--	-1		= From all stack
		--	0		= Current stack node (default)
		--	1...n	= Stack ID
)
RETURNS XML
AS BEGIN
	;DECLARE @LOG					INT	= 1
	;DECLARE @ERR_ACTION			INT	= 1
	;DECLARE @ERR_ACTION_CHLD		INT	= 1
	;DECLARE @ERR_WITH_INHERITED	INT	= 0
	;DECLARE @ERR_WHERE_TO_SEARCH	INT	= 0

	-- Set Default params
	;IF CHARINDEX(';',@CONFIG_STRING)=0
		SELECT	 @LOG					= ISNULL(SUM(IIF(s.[number]=1,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL)),@LOG)
				,@ERR_ACTION			= ISNULL(SUM(IIF(s.[number]=2,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL)),@ERR_ACTION)
				,@ERR_ACTION_CHLD		= ISNULL(SUM(IIF(s.[number]=3,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL)),@ERR_ACTION_CHLD)
				,@ERR_WITH_INHERITED	= ISNULL(SUM(IIF(s.[number]=4,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL)),@ERR_WITH_INHERITED)
				,@ERR_WHERE_TO_SEARCH	= ISNULL(SUM(IIF(s.[number]=5,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL)),@ERR_WHERE_TO_SEARCH)
		FROM q._split(@CONFIG_STRING,',') s
	ELSE
		SELECT	 @LOG					= ISNULL(SUM(CAST(q._param(s.[item],'log'					,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT)),@LOG)
				,@ERR_ACTION			= ISNULL(SUM(CAST(q._param(s.[item],'err_action'			,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT)),@ERR_ACTION)
				,@ERR_ACTION_CHLD		= ISNULL(SUM(CAST(q._param(s.[item],'err_action_chld'		,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT)),@ERR_ACTION_CHLD)
				,@ERR_WITH_INHERITED	= ISNULL(SUM(CAST(q._param(s.[item],'err_with_inherited'	,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT)),@ERR_WITH_INHERITED)
				,@ERR_WHERE_TO_SEARCH	= ISNULL(SUM(CAST(q._param(s.[item],'err_where_to_search'	,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT)),@ERR_WHERE_TO_SEARCH)
		FROM q._split(@CONFIG_STRING,';') s

	;RETURN (
			SELECT	 [@log]					= @LOG
					,[@err_action]			= @ERR_ACTION
					,[@err_action_chld]		= @ERR_ACTION_CHLD
					,[@err_with_inherited]	= @ERR_WITH_INHERITED
					,[@err_where_to_search]	= @ERR_WHERE_TO_SEARCH
					,[@config_string]		= CONCAT(@LOG,',',@ERR_ACTION,',',@ERR_ACTION_CHLD,',',@ERR_WITH_INHERITED,',',@ERR_WHERE_TO_SEARCH)
			FOR XML PATH('config'),TYPE
		)
END
GO
;EXEC q.sys_drop 1.05,'x_init'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-22 07:50
-- Update date: 2023-01-30 03:23
-- Version:		1.05
-- Description:	Create call stack on proc start
-- =============================================
CREATE FUNCTION q.x_init(
	 @PARENT		NVARCHAR(MAX)	= NULL
	,@CONFIG_STRING	NVARCHAR(MAX)	= NULL
)
RETURNS XML
AS BEGIN

	-- DECLARE
		;DECLARE @CUR_ID		INT
		;DECLARE @X				XML
		;DECLARE @XML			XML
		;DECLARE @GUID 			CHAR(36)
		;DECLARE @PARENT_NAME	NVARCHAR(255)

	-- PROCEDURE START
		-- Set Default params
		;SET @GUID		= q.guid()
		;SET @PARENT	= ISNULL(@PARENT,'Execution Start')
		
		-- Remove last semicolon
		;IF RIGHT(@PARENT,1)=';' SET @PARENT = REVERSE(STUFF(REVERSE(@PARENT),1,1,''))
		;SET @PARENT_NAME	= IIF(
				 CHARINDEX(';',@PARENT)=0
				,@PARENT
				,REVERSE(SUBSTRING(REVERSE(@PARENT),1,CHARINDEX(';',REVERSE(@PARENT))-1))
			)
		;SET @CUR_ID		= (SELECT COUNT(*) FROM q._split(@PARENT,';'))
		
		;SET @XML = (
				SELECT	 [@id]			= S.[number]
						,[@name]		= S.[item]
						,[@flow_guid]	= IIF(S.[number]=@CUR_ID,@GUID,NULL)
				FROM q._split(@PARENT,';') S
				FOR XML PATH('flow'),ROOT('flows'),TYPE
			)

		;SET @X	= (
				SELECT
					 [@id]				= @CUR_ID
					,[@name]			= @PARENT_NAME
					,[@type]			= IIF(@PARENT_NAME='Execution Start','INIT','UNDEFINED')
					,[@flow_guid]		= @GUID
					,[@session_id]		= @@SPID
					,[@session_guid]	= q.guid()
					,[@cur_id]			= @CUR_ID
					,[@start]			= GETDATE()
					,[@end]				= CAST(0 AS DATETIME)
					,[@duration]		= CAST(0 AS DATETIME)
					,[@error]			= 0
					,[@user]			= ORIGINAL_LOGIN()
					,[@host]			= HOST_NAME()
					,[result_state]		= 'running'
					,q.config(@CONFIG_STRING)
					,(
						SELECT	 [@stack_guid]	= q.guid()
								,(SELECT @PARENT FOR XML PATH('stack_string'),TYPE)
								,@XML
						FOR XML PATH('stack'),TYPE
					)
					,[subtasks]			= ''
					,[errors]			= ''
				FOR XML PATH('stack_task'),TYPE
			)

	-- RETURN
		;RETURN @X
;END
GO
;EXEC q.sys_drop 1.38,'x_start'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-30 17:40
-- Update date: 2023-02-03 16:20
-- Version:		1.38
-- Description:	Create call stack on proc start
-- =============================================
CREATE PROCEDURE q.x_start
	 @PROC_NAME_OR_ID	NVARCHAR(255)
	,@X					XML				= NULL	OUTPUT
	,@XX				XML				= NULL	OUTPUT
	,@CONFIG_STRING		NVARCHAR(MAX)	= NULL
	
	-- Support for older versions
	,@intDebug			INT				= NULL	OUTPUT
	,@strParent			NVARCHAR(MAX)	= NULL	OUTPUT
AS BEGIN
	;SET NOCOUNT ON

	-- DECLARE
		;DECLARE @PROC_ID	INT
		;DECLARE @CUR_ID	INT
		;DECLARE @XML_FLOWS	XML
		;DECLARE @XML_CONF	XML
		;DECLARE @GUID 		CHAR(36)
		;DECLARE @PROC_NAME	NVARCHAR(255)

		;DECLARE @br 		CHAR(2)	= CHAR(13)+CHAR(10)

	-- Log Flags
		;DECLARE @log_flag_exec		INT
		;DECLARE @log_flag_print	INT
		;DECLARE @log_flag_table	INT
		;DECLARE @log_flag_xml		INT
		;DECLARE @log_flag_hide		INT

	-- PROCEDURE START
	-- Set Log flags
		;SELECT
			 @log_flag_exec		= [log_flag_exec]
			,@log_flag_print	= [log_flag_print]
			,@log_flag_table	= [log_flag_table]
			,@log_flag_xml		= [log_flag_xml]
			,@log_flag_hide		= [log_flag_hide]
		FROM q.xflags_log

		;IF @X IS NULL
			SET @X = q.x_init(@strParent,@CONFIG_STRING)
		
		-- Validate XML
		;IF 0=ISNULL(q.x_xml_validate(@X),1)
			THROW 50000,'Call stack XML-variable is not valid',1 

		;SET @PROC_NAME = IIF(
				 ISNUMERIC(@PROC_NAME_OR_ID)=1
				,q.x_name(@PROC_NAME_OR_ID)
				,@PROC_NAME_OR_ID
			)
		;SET @PROC_ID = IIF(ISNUMERIC(@PROC_NAME_OR_ID)=1,@PROC_NAME_OR_ID,NULL)

		;SET @CUR_ID	= ISNULL(@X.value('/*[1]/@cur_id','INT'),0)+1
		;SET @GUID		= NEWID()
	
		;SELECT @XML_FLOWS	= (
				SELECT	x.query('(./flows/*)')
						,(SELECT [@id]=@CUR_ID,[@name]=@PROC_NAME,[@flow_guid]=@GUID FOR XML PATH('flow'),TYPE)
				FROM @X.nodes('/*[1]/stack[1]') a(x)
				FOR XML PATH(''),ROOT('flows'),TYPE
			)
		
		-- Update parent string including current step
		;SET @strParent = (STUFF((
				SELECT ';'+x.value('@name','NVARCHAR(MAX)')
				FROM @XML_FLOWS.nodes('/flows/*') a(x)
				ORDER BY x.value('@id','INT')
				FOR XML PATH('')
			),1,1,''))
		
		;SET @XML_CONF = (
				SELECT	 [@log]					= [log]
						,[@err_action]			= [err_action]
						,[@err_action_chld]		= [err_action_chld]
						,[@err_with_inherited]	= [err_with_inherited]
						,[@err_where_to_search]	= [err_where_to_search]
						,[@config_string]		= CONCAT([log],',',[err_action],',',[err_action_chld],',',[err_with_inherited],',',[err_where_to_search])
				FROM (
					SELECT	 [log]					= ISNULL([log]					,x.value('@log'					,'INT'))
							,[err_action]			= ISNULL([err_action]			,x.value('@err_action'			,'INT'))
							,[err_action_chld]		= ISNULL([err_action_chld]		,x.value('@err_action_chld'		,'INT'))
							,[err_with_inherited]	= ISNULL([err_with_inherited]	,x.value('@err_with_inherited'	,'INT'))
							,[err_where_to_search]	= ISNULL([err_where_to_search]	,x.value('@err_where_to_search'	,'INT'))
					FROM (
						SELECT * FROM (
							SELECT	 [log]					= SUM(IIF(s.[number]=1,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL))
									,[err_action]			= SUM(IIF(s.[number]=2,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL))
									,[err_action_chld]		= SUM(IIF(s.[number]=3,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL))
									,[err_with_inherited]	= SUM(IIF(s.[number]=4,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL))
									,[err_where_to_search]	= SUM(IIF(s.[number]=5,CAST(IIF(s.[item]='',NULL,s.[item]) AS INT),NULL))
							FROM q._split(@CONFIG_STRING,',') s
						) Q
						WHERE	CHARINDEX(';',ISNULL(@CONFIG_STRING,''))=0
							OR	@CONFIG_STRING	IS NULL
						
						UNION ALL
						
						SELECT * FROM (
							SELECT	 [log]					= SUM(CAST(q._param(s.[item],'log'					,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT))
									,[err_action]			= SUM(CAST(q._param(s.[item],'err_action'			,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT))
									,[err_action_chld]		= SUM(CAST(q._param(s.[item],'err_action_chld'		,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT))
									,[err_with_inherited]	= SUM(CAST(q._param(s.[item],'err_with_inherited'	,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT))
									,[err_where_to_search]	= SUM(CAST(q._param(s.[item],'err_where_to_search'	,DEFAULT,DEFAULT,DEFAULT,DEFAULT) AS INT))
							FROM q._split(@CONFIG_STRING,';') s
						) Q
						WHERE	CHARINDEX(';',ISNULL(@CONFIG_STRING,''))>0
							AND	@CONFIG_STRING	IS NOT NULL
					) Q
					OUTER APPLY @X.nodes('/*[1]/config[1]') a(x)
				) A
				FOR XML PATH('config'),TYPE
			)

		-- Set 
		;SET @XX	= (
				SELECT
					 [@id]				= @CUR_ID
					,[@name]			= @PROC_NAME
					,[@type]			= ISNULL((SELECT [type_desc] FROM sys.objects WITH (NOLOCK) WHERE [object_id] = @PROC_ID),'UNDEFINED')
					,[@flow_guid]		= @GUID
					,[@session_id]		= @@SPID
					,[@session_guid]	= @X.value('/*[1]/@session_guid','UNIQUEIDENTIFIER')
					,[@cur_id]			= @CUR_ID
					,[@start]			= GETDATE()
					,[@end]				= CAST(0 AS DATETIME)
					,[@duration]		= CAST(0 AS DATETIME)
					,[@error]			= 0
					,[@user]			= ORIGINAL_LOGIN()
					,[@host]			= HOST_NAME()
					,[@proc_id]			= @PROC_ID
					,[result_state]		= 'running'
					,@XML_CONF
					,(
						SELECT
							 [@stack_guid]	= NEWID()
							,(SELECT @strParent FOR XML PATH('stack_string'),TYPE)
							,@XML_FLOWS
						FOR XML PATH('stack'),TYPE
					)
					,[subtasks]			= ''
					,[errors]			= ''
				FOR XML PATH('stack_task'), TYPE
			)

		;IF q.xget_log(@X) & @log_flag_print >0 BEGIN
			;DECLARE @i			INT	= CHARINDEX('CREATE '+q.sys_obj_type_name(
					UPPER(ISNULL((SELECT [type] FROM sys.objects WITH (NOLOCK) WHERE [object_id] = @PROC_ID),'UNDEFINED'))
				),q.sys_def(@PROC_ID)) - 3
			;DECLARE @d			NVARCHAR(MAX)	= q._input('/*'+@br+'[*DEFINITION*]'+@br+'*/',q._str(NULL,'DEFINITION',LEFT(q.sys_def(@PROC_ID),IIF(@i>0,@i,0))))
			;PRINT @d
		;END
;END
GO
;EXEC q.sys_drop 1.34,'x_end'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2022-12-30 17:42
-- Update date: 2023-01-30 08:40
-- Version:		1.34
-- Description:	Close call stack on proc end
-- =============================================
CREATE PROCEDURE q.x_end
	 @X		XML	OUTPUT
	,@XX	XML
	-- XML with Additional Info
	,@Xi	XML = NULL
AS BEGIN
	;SET NOCOUNT ON

	-- DECLARE
		;DECLARE @current_id	INT
		;DECLARE @XML			XML
		;DECLARE @dt_duration	DATETIME
		;DECLARE @dt_start		DATETIME
		;DECLARE @dt_end		DATETIME	= GETDATE()

	-- Log Flags
		;DECLARE @log_flag_exec		INT
		;DECLARE @log_flag_print	INT
		;DECLARE @log_flag_table	INT
		;DECLARE @log_flag_xml		INT
		;DECLARE @log_flag_hide		INT

	-- PROCEDURE START
		-- Validate XML
		;IF 0=ISNULL(q.x_xml_validate(@X),1)
			THROW 50000,'Call stack XML-variable is not valid' ,1
		
		-- Set Log flags
		;SELECT
			 @log_flag_exec		= [log_flag_exec]
			,@log_flag_print	= [log_flag_print]
			,@log_flag_table	= [log_flag_table]
			,@log_flag_xml		= [log_flag_xml]
			,@log_flag_hide		= [log_flag_hide]
		FROM q.xflags_log

		;SELECT	 @dt_start		= ISNULL(x.value('@start'	,'DATETIME'),@dt_end)
				,@current_id	= ISNULL(x.value('@cur_id'	,'INT')		,0)
		FROM @XX.nodes('/*[1]') a(x)
		
		;SET @dt_duration	= @dt_end-@dt_start

		;SET @XX.modify('replace value of (/*[1]/@end) with sql:variable("@dt_end")')
		;SET @XX.modify('replace value of (/*[1]/@duration) with sql:variable("@dt_duration")')

		-- Set Error flag from child
		;IF @XX.value('/*[1]/@error','INT') = 1 BEGIN
			;SET @X.modify('replace value of (/*[1]/@error) with 1')

			-- Add Errors from child
			;SET @XML = (
					SELECT x.query('(./errors/*)')
					FROM @XX.nodes('/*[1]') b(x)
					FOR XML PATH(''),TYPE
				)

			-- Set Inherited flag
			;DECLARE @err_cnt INT = @XML.value('count(/error)', 'INT')
			;WHILE @err_cnt>0 BEGIN
				;SET @XML.modify('replace value of (/*[sql:variable("@err_cnt")]/@err_is_inherited)[1] with 1')
				;SET @err_cnt -= 1
			;END
			;SET @X.modify('insert sql:variable("@XML") into (/*[1]/errors)[1]')
		;END

		-- Insert/Remove additional info in case of Debug or Errors
		;IF	q.xget_log(@X) & @log_flag_hide >0
		AND	@XX.value('/*[1]/@error','INT')=0
			BEGIN
				;SET @XX.modify('delete /*[1]/result_state')
				;SET @XX.modify('delete /*[1]/stack')
				;SET @XX.modify('delete /*[1]/errors')
				;IF @XX.value('count(/*[1]/subtasks/*)', 'INT')=0
					SET @XX.modify('delete /*[1]/subtasks')
			END
		ELSE
			BEGIN
				;SET @XX.modify('insert <info/> as last into (/*[1])[1]')
				;SET @XX.modify('insert sql:variable("@Xi") into (/*[1]/info)[1]')
				;SET @XX.modify('replace value of (/*[1]/result_state/text())[1] with (if (count(/*[1]/errors/error)=0) then "OK" else "Error")')
			END


		-- Set Exec duration
		;SET @dt_start		= ISNULL(@X.value('/*[1]/@start','DATETIME'),@dt_end)
		;SET @dt_duration	= @dt_end-@dt_start

		;SET @X.modify('replace value of (/*[1]/@end) with sql:variable("@dt_end")')
		;SET @X.modify('replace value of (/*[1]/@duration) with sql:variable("@dt_duration")')
		;SET @X.modify('replace value of (/*[1]/@cur_id) with sql:variable("@current_id")')
		;SET @X.modify('insert sql:variable("@XX") into (/*[1]/subtasks)[1]')
		;SET @X.modify('replace value of (/*[1]/result_state/text())[1] with (if (count(/*[1]/errors/error)=0) then "OK" else "Error")')

		;EXEC q.x_err_handler @X
;END
GO
;EXEC q.sys_drop 1.13,'x_err2xml'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-19 19:10
-- Update date:	2023-01-24 19:00
-- Version:		1.13
-- Description:	Convert err data in XML-data
-- =============================================
CREATE FUNCTION q.x_err2xml(
	 @X					XML
	,@ERROR_NUMBER		INT
	,@ERROR_MESSAGE		NVARCHAR(MAX)
	,@ERROR_LINE		INT		= NULL
	,@ERROR_SEVERITY	INT		= NULL
	,@ERROR_STATE		INT		= NULL
	,@ERROR_PROCEDURE	SYSNAME	= NULL
)
RETURNS XML
AS BEGIN
	-- DECLARE
		;DECLARE @ERR_XML	XML

	-- PROCEDURE START
		-- Validate XML
		;IF 0=ISNULL(q.x_xml_validate(@X),1)
			SET @X = NULL
	
		;IF	ISNULL(@ERROR_NUMBER	,0)		!= 0
		AND	ISNULL(@ERROR_MESSAGE	,'')	!= ''
		BEGIN
			;SET @ERR_XML = (
				SELECT	 [@err_number]			= @ERROR_NUMBER
						,[@err_message]			= @ERROR_MESSAGE
						,[@err_line]			= @ERROR_LINE
						,[@err_severity]		= @ERROR_SEVERITY
						,[@err_state]			= @ERROR_STATE
						,[@err_procedure]		= @ERROR_PROCEDURE
						,[@err_datetime]		= GETDATE()
						,[@err_guid]			= q.guid()
						,[@err_is_inherited]	= 0
						,[@flow_guid]			= @X.value('/*[1]/@flow_guid'			,'CHAR(36)')
						,[@stack_guid]			= @X.value('/*[1]/stack[1]/@stack_guid'	,'CHAR(36)')
				FOR XML PATH('error'),TYPE
			)
		;END

	;RETURN @ERR_XML
;END
GO
;EXEC q.sys_drop 1.0,'x_err_add'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-25 06:00
-- Version:		1.0
-- Description:	Add error in call stack XML-data
-- =============================================
CREATE PROCEDURE q.x_err_add
	 @X				XML OUTPUT
	,@ERR			XML
	,@ERR_ACTION	INT	= NULL
AS BEGIN
	-- DECLARE

	-- PROCEDURE START
		-- Validate XML
		;IF 0=ISNULL(q.x_xml_validate(@X),1)
			SET @X = NULL

		-- Set Error flag = 1 and add Err details
		;SET @X.modify('replace value of (/*[1]/@error) with 1')
		;SET @X.modify('insert sql:variable("@ERR") into (/*[1]/errors)[1]')

		;EXEC q.x_err_handler @X,@ERR_ACTION
;END
GO
;EXEC q.sys_drop 1.22,'_exec'
GO
-- =============================================
-- Author:		Hegel Maxim
-- Create date: 2023-01-23 00:30
-- Update date: 2023-02-02 20:00
-- Version:		1.22
-- Description:	RUN Dynamic SQL
-- =============================================
CREATE PROCEDURE q._exec
	 @Q		NVARCHAR(MAX)	= NULL
	,@DB	SYSNAME			= NULL

	,@X		XML				= NULL OUTPUT
WITH EXECUTE AS OWNER
AS BEGIN
	;EXECUTE AS CALLER

	-- PROCEDURE HINTS
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE HINTS >>>>>>>>>>>>
			-- Specifies that statements can read rows that have been
			-- modified by other transactions but not yet committed
			;SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
		
			-- Specifies whether SQL Server automatically rolls back the 
			-- current transaction when a Transact-SQL statement raises
			-- a run-time error.
			;SET XACT_ABORT ON
		
			-- Stops the message that shows the count of the number of
			-- rows affected by a Transact-SQL statement or stored 
			-- procedure from being returned as part of the result set.
			;SET NOCOUNT ON
		;END	-- <<<<<<<<<<<< PROCEDURE HINTS <<<<<<<<<<<<
	
	-- PROCEDURE START
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE START >>>>>>>>>>>>
			;DECLARE @RC INT=0,@XX XML,@Xi XML,@xERR XML,@XEA INT
			;EXEC q.x_start @@PROCID,@X=@X OUTPUT,@XX=@XX OUTPUT
			;SET @XEA=q.x_err_action(@X)
		;END	-- <<<<<<<<<<<< PROCEDURE START <<<<<<<<<<<<

	-- DECLARE
		;BEGIN	-- >>>>>>>>>>>> DECLARE >>>>>>>>>>>>
			;DECLARE @br	CHAR(2)	= CHAR(13)+CHAR(10)
			
			;DECLARE @X_TMP XML, @OLD_EA INT,@OLD_EAC INT,@NEW_EA INT,@NEW_EAC INT
			
			;DECLARE @strErrMsg		NVARCHAR(255)
			;DECLARE @guidQ			NVARCHAR(8)		= LEFT(NEWID(),8)
			;DECLARE @guidQE		NVARCHAR(8)		= LEFT(NEWID(),8)
			;DECLARE @QPROC			SYSNAME
			;DECLARE @QXi			NVARCHAR(255)
			;DECLARE @QE			NVARCHAR(MAX)
			;DECLARE @SQL			NVARCHAR(MAX)
			;DECLARE @tblExecResult	TABLE(
					 [id]			INT IDENTITY(1,1)
					,[output]		NVARCHAR(MAX)
				)

		-- Log Flags
			;DECLARE @log_flag_exec		INT
			;DECLARE @log_flag_print	INT
			;DECLARE @log_flag_table	INT
			;DECLARE @log_flag_xml		INT
			;DECLARE @log_flag_hide		INT

		-- Error flags
			;DECLARE @err_flag_throw	INT
			;DECLARE @err_flag_print	INT
			;DECLARE @err_flag_table	INT
			;DECLARE @err_flag_select	INT
			;DECLARE @err_flag_email	INT
		;END	-- <<<<<<<<<<<< DECLARE <<<<<<<<<<<<

    -- MAIN PART
		;BEGIN	-- >>>>>>>>>>>> MAIN PART >>>>>>>>>>>>
			;BEGIN TRY	-- >>>>>>>>>>>> MAIN TRY >>>>>>>>>>>>
			-- Set Log flags
				;SELECT
					 @log_flag_exec		= [log_flag_exec]
					,@log_flag_print	= [log_flag_print]
					,@log_flag_table	= [log_flag_table]
					,@log_flag_xml		= [log_flag_xml]
					,@log_flag_hide		= [log_flag_hide]
				FROM q.xflags_log

			-- Set Error flags
			;SELECT
				 @err_flag_throw	= [err_flag_throw]
				,@err_flag_print	= [err_flag_print]
				,@err_flag_table	= [err_flag_table]
				,@err_flag_select	= [err_flag_select]
				,@err_flag_email	= [err_flag_email]
			FROM q.xflags_err

				;SET @DB	= ISNULL(q.sys_unquote(@DB),DB_NAME())
				;SET @Q		= REPLACE(@Q,'##temp##',@guidQ)

				-- Print to console
				;IF q.xget_log(@XX) & @log_flag_print >0 BEGIN
					;DECLARE @intEndPos		INT
					;DECLARE @intFind		INT
					;DECLARE @strQueryPart	NVARCHAR(MAX)
					
					;SET @SQL = @Q

					;IF @DB IS NOT NULL
						PRINT ';USE ['+@DB+']'+@br+'GO'+@br
	
					;WHILE LEN(@SQL) > 0 BEGIN
						;IF LEN(@SQL) > 4000 BEGIN
							;SET @strQueryPart	= LEFT(@SQL, 4000)
							;SET @intFind		= CHARINDEX(CHAR(10), REVERSE(@strQueryPart))
							;SET @intEndPos		= LEN(@strQueryPart) - @intFind + IIF(@intFind = 0, 0, 1)
							;SET @strQueryPart	= SUBSTRING(@SQL, 0, @intEndPos)
									
							;PRINT @strQueryPart
									
							;SET @SQL		= RIGHT(@SQL, LEN(@SQL) - @intEndPos)
						;END ELSE BEGIN
							;PRINT @SQL
							;SET @SQL = ''
						;END
					;END

					;PRINT 'GO'+@br
				;END
				
				-- Write query to XML if not executed
				;IF q.xget_log(@XX) & @log_flag_xml	 >0
				AND q.xget_log(@XX) & @log_flag_exec =0 BEGIN
					;SET @X_TMP = (
							SELECT	 [@start]		= GETDATE()
									,[@end]			= NULL
									,[@duration]	= NULL
									,[@is_executed]	= 0
									,@Q
							FOR XML PATH('query'),ROOT('queries'),TYPE
						)
					;SET @XX.modify('insert sql:variable("@X_TMP") into (/*[1])')
				;END

				-- Write query in a table if not executed
				;IF q.xget_log(@XX) & @log_flag_table	>0
				AND q.xget_log(@XX) & @log_flag_exec	=0 BEGIN
					;INSERT INTO q.tlog_queries([query],[user_name],[host],[start],[db_name],[session_id],[session_guid],[flow_guid],[stack_string])
					 SELECT @Q,ORIGINAL_LOGIN(),HOST_NAME(),GETDATE(),@DB,@@SPID,q.xget_session_guid(@XX),q.xget_flow_guid(@XX),q.xget_parent(@XX)
				;END

				-- Exec query
				;IF q.xget_log(@XX) & @log_flag_exec >0 BEGIN
					;SET @QPROC		= q._input('[_tmp_dynamic].[sql_[!SESSION_GUID!]_[!FLOW_ID!]_[!TEMP_GUID!]]'
						,q._int(q._str(q._str(NULL
							,'TEMP_GUID'	,@guidQE)
							,'SESSION_GUID'	,LEFT(q.xget_session_guid(@XX),8))
							,'FLOW_ID'		,q.x_last_id(@XX)+1)
					)
					
					;IF @DB=DB_NAME() BEGIN
						;SET @QE		= @Q

						;IF CHARINDEX('DECLARE@Xi_'+@guidQ+'XML',REPLACE(REPLACE(@Q,' ',''),CHAR(9),''))>0
							SET @QXi	= ';SET @Xi_#$temp$# = @Xi_'+@guidQ
					END ELSE BEGIN
						;SET @strErrMsg = REPLACE('Framework Q is not intalled on database [@]','@',@DB)
						;IF OBJECT_ID(@DB+'.q._exec') IS NULL
							THROW 50000,@strErrMsg,1

						;SET @QE	= q._input(';EXEC [{DB_NAME}].q._exec @Q_#$temp$#,@X=@XX_#$temp$# OUT',q._str(NULL,'DB_NAME',@DB))
					;END

					;SET @QE		= REPLACE(REPLACE(REPLACE(
							 q._get(NULL,@@PROCID
							,q._datetime(q._str(q._str(q._str(q._str(NULL
								,'SYS_IN_PROC_NAME'		,@QPROC)
								,'SYS_IN_SET_Xi'		,ISNULL(@QXi,''))
								,'SYS_IN_SESSION_ID'	,LEFT(q.xget_session_guid(@XX),8))
								,'SYS_IN_USER_NAME'		,ORIGINAL_LOGIN())
								,'SYS_IN_DATETIME'		,GETDATE())
							)
							,'[!SYS_IN_QUERY_TEXT!]',@QE)
							,'#$temp$#',@guidQE)
							,'##temp##',@guidQ)
					
					-- Create temp procedure
					;REVERT
						;EXEC (@QE)	-- Should be executed as Administrator
					;EXECUTE AS CALLER

					-- Run temp procedure
					;EXEC @QPROC @Q,@XX OUTPUT

					-- Drop temp procedure
					;IF OBJECT_ID(@QPROC) IS NOT NULL BEGIN
						;REVERT
							;EXEC ('DROP PROCEDURE '+@QPROC) -- Should be executed as Administrator
						;EXECUTE AS CALLER
					;END
				;END
			;END TRY	-- <<<<<<<<<<<< MAIN TRY <<<<<<<<<<<<
			BEGIN CATCH	-- >>>>>>>>>>>> MAIN CATCH >>>>>>>>>>>>
				-- Collect Error info
				;SET @xERR = q.x_err2xml(@XX,ERROR_NUMBER(),ERROR_MESSAGE(),ERROR_LINE(),ERROR_SEVERITY(),ERROR_STATE(),ERROR_PROCEDURE())
				
				-- Drop temp procedure
				;IF OBJECT_ID(@QPROC) IS NOT NULL BEGIN
					;REVERT
						;EXEC ('DROP PROCEDURE '+@QPROC) -- Should be executed as Administrator
					;EXECUTE AS CALLER
				;END

				-- Add error to XML
				;EXEC q.x_err_add @XX OUT,@xERR,@XEA
			;END CATCH	-- <<<<<<<<<<<< MAIN CATCH <<<<<<<<<<<<
		;END	-- <<<<<<<<<<<< MAIN PART <<<<<<<<<<<<

	-- PROCEDURE END
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE END >>>>>>>>>>>>
			;EXEC q.x_end @X OUTPUT,@XX,@Xi
		;END	-- <<<<<<<<<<<< PROCEDURE END <<<<<<<<<<<<

	-- RETURN
		;RETURN q.xget_err_status(@X)

	-- SAVED QUERIES
		;BEGIN	-- >>>>>>>>>>>> SAVED QUERIES >>>>>>>>>>>>
/*≡≡≡--Query--≡≡≡
-- =============================================
-- Author:		[!SYS_IN_USER_NAME!]
-- Create date: [!SYS_IN_DATETIME!]
-- Description:	Dynamic SQL
--				session_id:	[!SYS_IN_SESSION_ID!]
--				id:			#$temp$#
--				temp:		##temp##
-- =============================================
CREATE PROCEDURE [!SYS_IN_PROC_NAME!]
	 @Q_#$temp$#	NVARCHAR(MAX)	= NULL
	,@X_#$temp$#	XML				= NULL OUTPUT
AS BEGIN
	-- PROCEDURE HINTS
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE HINTS >>>>>>>>>>>>
			-- Specifies that statements can read rows that have been
			-- modified by other transactions but not yet committed
			;SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
		
			-- Specifies whether SQL Server automatically rolls back the 
			-- current transaction when a Transact-SQL statement raises
			-- a run-time error.
			;SET XACT_ABORT ON
		
			-- Stops the message that shows the count of the number of
			-- rows affected by a Transact-SQL statement or stored 
			-- procedure from being returned as part of the result set.
			;SET NOCOUNT ON
		;END	-- <<<<<<<<<<<< PROCEDURE HINTS <<<<<<<<<<<<
	
	-- PROCEDURE START
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE START >>>>>>>>>>>>
			;DECLARE @RC_#$temp$# INT=0,@XX_#$temp$# XML,@Xi_#$temp$# XML,@xERR_#$temp$# XML,@XEA_#$temp$# INT
			;EXEC q.x_start @@PROCID,@X=@X_#$temp$# OUTPUT,@XX=@XX_#$temp$# OUTPUT
			;SET @XEA_#$temp$#=q.x_err_action(@X_#$temp$#)
		;END	-- <<<<<<<<<<<< PROCEDURE START <<<<<<<<<<<<
		
	-- DECLARE
		;BEGIN	-- >>>>>>>>>>>> DECLARE >>>>>>>>>>>>
			;DECLARE @X_TMP_#$temp$#			XML
			;DECLARE @dtSart_#$temp$#			DATETIME
			;DECLARE @dtEnd_#$temp$#			DATETIME
			
			;DECLARE @SQL_#$temp$#				NVARCHAR(MAX)
			;DECLARE @definition_#$temp$#		NVARCHAR(MAX)
			;DECLARE @searchStart_#$temp$#		NVARCHAR(255)
			;DECLARE @searchEnd_#$temp$#		NVARCHAR(255)

			;DECLARE @log_flag_table_#$temp$#	INT
			;DECLARE @log_flag_xml_#$temp$#		INT
		;END	-- <<<<<<<<<<<< DECLARE <<<<<<<<<<<<

    -- MAIN PART
		;BEGIN	-- >>>>>>>>>>>> MAIN PART >>>>>>>>>>>>
			;BEGIN TRY	-- >>>>>>>>>>>> MAIN TRY >>>>>>>>>>>>
				;SELECT	 @log_flag_table_#$temp$#	= [log_flag_table]
						,@log_flag_xml_#$temp$#		= [log_flag_xml]
				FROM q.xflags_log

				;SET @searchStart_#$temp$#	= REPLACE(N'--≡≡≡--DYNAMIC SQL #$temp$# @--≡≡≡','@','START')
				;SET @searchEnd_#$temp$#	= REPLACE(@searchStart_#$temp$#,'START','END')
				;SET @definition_#$temp$#	= q.sys_def(@@PROCID)
				;SET @definition_#$temp$#	= SUBSTRING(@definition_#$temp$#,CHARINDEX(@searchStart_#$temp$#,@definition_#$temp$#)+2+LEN(@searchStart_#$temp$#),LEN(@definition_#$temp$#))
				;SET @SQL_#$temp$#			= LEFT(@definition_#$temp$#,CHARINDEX(@searchEnd_#$temp$#,@definition_#$temp$#)-1)
				;SET @dtSart_#$temp$#		= GETDATE()

--≡≡≡--DYNAMIC SQL #$temp$# START--≡≡≡
[!SYS_IN_QUERY_TEXT!]
--≡≡≡--DYNAMIC SQL #$temp$# END--≡≡≡

				[!SYS_IN_SET_Xi!]
				;SET @dtEnd_#$temp$# = GETDATE()
			;END TRY	-- <<<<<<<<<<<< MAIN TRY <<<<<<<<<<<<
			BEGIN CATCH	-- >>>>>>>>>>>> MAIN CATCH >>>>>>>>>>>>
				;SET @dtEnd_#$temp$# = GETDATE()
				;SET @xERR_#$temp$# = q.x_err2xml(@XX_#$temp$#,ERROR_NUMBER(),ERROR_MESSAGE(),ERROR_LINE(),ERROR_SEVERITY(),ERROR_STATE(),ERROR_PROCEDURE())
				;EXEC q.x_err_add @XX_#$temp$# OUT,@xERR_#$temp$#,@XEA_#$temp$#
			;END CATCH	-- <<<<<<<<<<<< MAIN CATCH <<<<<<<<<<<<
		;END	-- <<<<<<<<<<<< MAIN PART <<<<<<<<<<<<

	-- ADD INFO IN XML
		;BEGIN TRY	-- >>>>>>>>>>>> XML TRY  >>>>>>>>>>>>
			;IF	q.xget_log(@XX_#$temp$#) & @log_flag_table_#$temp$# >0 BEGIN
				;INSERT INTO q.tlog_queries([query],[user_name],[host],[db_name],[start],[end],[duration],[session_id],[session_guid],[flow_guid],[stack_string])
				 SELECT @SQL_#$temp$#,ORIGINAL_LOGIN(),HOST_NAME(),DB_NAME(),@dtSart_#$temp$#,@dtEnd_#$temp$#,@dtEnd_#$temp$#-@dtSart_#$temp$#,@@SPID,q.xget_session_guid(@XX_#$temp$#),q.xget_flow_guid(@XX_#$temp$#),q.xget_parent(@XX_#$temp$#)
			;END

			;IF	q.xget_err_status(@XX_#$temp$#)	= 1
			OR	q.xget_log(@XX_#$temp$#) & @log_flag_xml_#$temp$#	>0
			BEGIN
				;SET @X_TMP_#$temp$# = (
						SELECT	 [@start]		= @dtSart_#$temp$#
								,[@end]			= @dtEnd_#$temp$#
								,[@duration]	= @dtEnd_#$temp$#-@dtSart_#$temp$#
								,[@is_executed]	= IIF(q.xget_err_status(@XX_#$temp$#)=1,0,1)
								,@SQL_#$temp$#
						FOR XML PATH('query'),ROOT('queries'),TYPE
					)
				;SET @XX_#$temp$#.modify('insert sql:variable("@X_TMP_#$temp$#") into (/*[1])')
			;END
		;END TRY	-- <<<<<<<<<<<< XML TRY <<<<<<<<<<<<
		BEGIN CATCH	-- >>>>>>>>>>>> XML CATCH >>>>>>>>>>>>
			;SET @xERR_#$temp$# = q.x_err2xml(@XX_#$temp$#,ERROR_NUMBER(),ERROR_MESSAGE(),ERROR_LINE(),ERROR_SEVERITY(),ERROR_STATE(),ERROR_PROCEDURE())
			;EXEC q.x_err_add @XX_#$temp$# OUT,@xERR_#$temp$#,@XEA_#$temp$#
		;END CATCH	-- <<<<<<<<<<<< XML CATCH <<<<<<<<<<<<

	-- PROCEDURE END
		;BEGIN	-- >>>>>>>>>>>> PROCEDURE END >>>>>>>>>>>>
			;EXEC q.x_end @X_#$temp$# OUTPUT,@XX_#$temp$#,@Xi_#$temp$#
		;END	-- <<<<<<<<<<<< PROCEDURE END <<<<<<<<<<<<

	-- RETURN
		;RETURN q.xget_err_status(@X_#$temp$#)
;END
--≡≡≡*/*/
			;DECLARE @savedqueries INT
		;END	-- <<<<<<<<<<<< SAVED QUERIES <<<<<<<<<<<<
;END
GO
