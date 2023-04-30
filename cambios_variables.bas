

' cambia una variable tipo "float" como "single" (o int como integer o lo que sea)
function cambia_variable(ByVal cad As string) As String
		Dim cad2 As String=""
		'Dim cad3 As String=""

		'If Right(cad,1)<>" " Then cad=cad+" " ' por si acaso.....
		cad=Trim(cad)
		cad2=cad ' si no s reconoce la variable, se devuelve la misma de entrada
		
		'Print "ENTRADA:";cad;"--";cad2:sleep
		
		'cad2="REVISAR_"+cad ' en casos desconocidos, para luego cambiar a mano en BASIC
		'cad2=cad ' lo dejo sin tocar por ahora, me resulta mas comodo
		'If InStr(cad,"unsigned ") Or Left(cad,1)="u" Then cad2="U"':cad="" Else cad3="":cad="" ' casos unsigned		
		'If cad="unsigned" Then cad2="UInteger":cad="" ' en caso de omision de tipo, y qus solo lleva un "unsigned" a pelo
		
		if cad="unsigned int" Then cad2="UInteger" : cad=""
		if cad="unsigned short" Then cad2="UShort" : cad=""
		if cad="unsigned char" Then cad2="UByte" : cad=""
		if cad="unsigned long" Then cad2="ULong" : cad=""
		 
		if cad="signed int" Then cad2="Integer" : cad=""  
		if cad="signed short" Then cad2="Short" : cad=""  
		if cad="signed char" Then cad2="Byte" : cad=""  
		if cad="signed long" Then cad2="Long" : cad=""
		 
		'if cad="int)" Then cad2="Integer)" : cad=""
		'if cad="short)" Then cad2="Short)" : cad=""
		'if cad="char)" Then cad2="Byte)" : cad=""
		'if cad="long)" Then cad2="Long)" : cad=""
		 
		'if cad="uint)" Then cad2="UInteger)" : cad=""
		'if cad="ushort)" Then cad2="Ushort)" : cad=""
		'if cad="uchar)" Then cad2="UByte)" : cad=""
		'if cad="ulong)" Then cad2="ULong)" : cad=""
		 
		if cad="uint_8" Then cad2="UByte" : cad=""
		if cad="uint_16" Then cad2="UShort" : cad=""
		if cad="uint_32" Then cad2="UInteger" : cad=""
		if cad="uint_64" Then cad2="ULong" : cad=""
		 
		if cad="int_8" Then cad2="Byte" : cad=""
		if cad="int_16" Then cad2="Short" : cad=""
		if cad="int_32" Then cad2="Integer" : cad=""
		if cad="int_64" Then cad2="Long" : cad=""
		 
		if cad="uint8" Then cad2="UByte" : cad=""
		if cad="uint16" Then cad2="UShort" : cad=""
		if cad="uint32" Then cad2="UInteger" : cad=""
		if cad="uint64" Then cad2="ULong" : cad=""
		 
		if cad="int8" Then cad2="Byte" : cad=""
		if cad="int16" Then cad2="Short" : cad=""
		if cad="int32" Then cad2="Integer" : cad=""
		if cad="int64" Then cad2="Long" : cad=""
		 
		if cad="uint" Then cad2="UInteger" : cad=""
		if cad="ushort" Then cad2="UShort" : cad=""
		if cad="uchar" Then cad2="UByte" : cad=""
		if cad="ulong" Then cad2="ULong" : cad=""
		 
		if cad="int" Then cad2="Integer" : cad=""
		if cad="short" Then cad2="Short" : cad=""
		if cad="char" Then cad2="Byte" : cad=""
		if cad="long" Then cad2="Long" : cad=""
		 
		if cad="float" Then cad2="Single" : cad=""
		if cad="double" Then cad2="Double" : cad=""
		
		
		If LCase(cad)="lpvoid" Then cad2="lpVoid":cad="" ' LPVOID lo dejo
		If LCase(cad)="void"   Then cad2="":cad="" ' VOID se elimina
	
		
	cambia_variable=cad2'+cad2
	'Print "SALIDA:";cad3+cad2;"--"
End Function






tipos_de_variables:
' es importante buscar segun la longitud, de mayor a menor, para evitar que una busqueda corta, altere una larga
' ejemplo, si busco "int" en "uint8" y lo arreglo, queda como "uinteger8", con lo cual, cuando llegue a la busqueda del "uint8", no lo ecuentra
Data "unsigned int","unsigned short","unsigned char","unsigned long"
Data   "signed int",  "signed short",  "signed char",  "signed long"

Data "int)","short)","char)","long)" ' estos casos menor no tratar por ahora
Data "uint)","ushort)","uchar)","ulong)"

Data "uint_8","uint_16","uint_32","uint_64"
Data "int_8","int_16","int_32","int_64"

Data "uint8","uint16","uint32","uint64"
Data "int8","int16","int32","int64"

Data "uint","ushort","uchar","ulong"
Data "int","short","char","long"

Data "float","double"

Data "fin"

' localiza variables NO cambiadas antes de llegar aqui, en concreto, las de DENTRO de los SUB
Sub busca_variables()
	Dim As Integer aa,bb,lenvar
	Dim As String vervariable=""

	P=1 ' contador

	Restore tipos_de_variables

	Print "Cambiando variables tipo C a BASIC"
	
	While vervariable<>"fin"
		Read vervariable
		lenvar=Len(vervariable) 
		While 1
			
			' leemos uno a uno los caracteres. 
			sb=leetexto(P,1)
			bb=0:If Mid(tcpp,P,lenvar)=vervariable Then bb=P ' si la variable coincide en sus letras, podemos ir a verificar

			If bb>0 And (tipo<CRLF) And nllave>0 Then ' hay variable, no es comentario y estamos dentro de "{}" de un SUB
				' cojo uno antes y otro despues, para estudiar casos como una variable de nombre HFLOAT, que confundo con un tipo FLOAT
				If Right(vervariable,1)=")" Then lenvar-=1 ' si es un tipo forzado con "(var)", quito el parentesis final ")" de la longitud
				sb=leetexto(bb-1,lenvar+2) ' cojo DOS de mas, uno delante, otro detras
				'''''
				' analizo el anterior y el posterior, por si es falsa alarma. podria ser real, pero no en esta busqueda
				' o sea, podriamos estar buscando INT, pero encontrar un UINT8, que tambien vale, pero NO en esta busqueda, esta es para INT
				sc=LCase(Left(sb,1)) ' caracter anterior
				If Asc(sc)>96 And Asc(sc)<123 Or sa="_" Then sb="" ' falsa alarma, salimos para seguir buscando
				sc=LCase(Right(sb,1)) ' caracter posterior
				If Asc(sc)>96 And Asc(sc)<123 Or sa="_" Then sb="" ' falsa alarma, salimos para seguir buscando
				'''''
				If Right(sb,1)=")" Then sb="" ' los casos "forzados" encerrados en () tipo '(int)' '(short)', etc, no los trato aun
				If sb<>"" Then
					sc="Dim As "+cambia_variable(vervariable) ' dentro de un SUB, NO lleva SHARED
					''''Print Mid(tcpp,P-20,40):Print sc:sleep
					borratexto(bb,lenvar) ' quito lo viejo entero, para luego hacer sitio para lo nuevo
					insertatexto(bb,Len(sc))
					cambiatexto(bb,sc)
				End If
			EndIf
			P+=1
			If P>tcar Then Exit while

		Wend
		P=1 ' a por otra variable
	Wend

End Sub





' invertimos un conjunto completo de parametros entre "()" 
Function invierte_parametros(ByVal params As String) As String
	Dim As Integer aa,ff,gg,hh
	Dim As String sa,sb,sc,sd
	Dim As String comandos(100)
	
	' quito los parentesis delantero y trasero
	'params=Mid(params,2,Len(params)-2)

	' quito posibles CRLF y TAB, para hacer una linea unica
	sa=""
	For ff=1 To Len(params) ' buca comas que separen los parametros
		sb=Mid(params,ff,1)
		If sb=Chr(CRLF) Or sb=Chr(9) Then sb=""
		sa=sa+sb		
	Next
	params=sa ' y se queda como antes, pero mas limpito

	'Print params

	' busca comas que separen los parametros
	gg=0
	For ff=1 To 100
		aa=InStr(params,",") 
		gg+=1
		If aa=0 Then comandos(gg)=Trim(params):Exit For	
		comandos(gg)=Trim(Left(params,aa-1))
		params=Mid(params,aa+1)
	Next
	
	' y los invierto uno a uno
	For ff=1 To gg
		sa=comandos(ff)
		
		' verifico si es un puntero
		 hh=InStr(sa,"*") 
		 If hh Then 
		 	sd=" Ptr" 
		 	Mid(sa,hh,1)=" " ' elimino el "*"
		 Else 
		 	sd=""
		 EndIf
		 
		aa=InStr(sa," ")
		sb=Left(sa,aa-1) ' este es el tipo de variable
		sb=cambia_variable(sb) ' la cambiamos de C a FB
		sa=mid(sa,aa+1) ' este es el nombre
		sc=sc+sa+" As "+sb+sd ' y recontruyo la variable
		If ff<gg Then sc=sc+" , " ' sumo la coma, mientras no sea la ultima
	Next
	params=sc
	If params=" As " Then params="" ' si no hay parametros, quito el "As" solitario
	
	' le devuelvo los parentesis al conjunto reparado
	'params="( "+params+" )"
	'Print params
	'sleep
	Return params
End Function

