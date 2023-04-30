#Include "variables.bas"
#Include "ficheros.bas"
#Include "cambios_basicos.bas"
#include "cambios_variables.bas"
#include "comandos.bas"
#include "textos.bas"
	
	
	
	
	
	
	
fich_entrada=Command
If fich_entrada="" Then fich_entrada="lines.c"

' separo ruta de nombre, si existe ruta
fich_ruta=""
If InStr(fich_entrada,"\") Then
	For a=Len(fich_entrada) To 1 Step -1
		If Mid(fich_entrada,a,1)="\" Then Exit For
	Next
	fich_ruta=Left(fich_entrada,a)
	fich_entrada=Mid(fich_entrada,a+1)
EndIf


If InStr(fich_entrada,".")=0 Then Print "Sin extension de fichero, no hay conversion...":Sleep:End
a=InStr(fich_entrada,".")
sa=LCase(Mid(fich_entrada,a+1)) ' me quedo con la extension
If Left(sa,1)="c" Then fich_salida=fich_ruta + Left(fich_entrada,a)+"bas"
If Left(sa,1)="h" Then fich_salida=fich_ruta + Left(fich_entrada,a)+"bi"
fich_entrada=fich_ruta + fich_entrada
'If fich_entrada="" Then End 



	
leer_fichero()

' para depurar, activar modo grafico, 
'ScreenRes 1024,768,32


' ------------- OJO CON CAMBIAR EL ORDEN DE LAS LLAMADAS, ESTAN ASI POR ALGO !!! ----------------

' marca grupos de comentarios, comillas y defines (NOTA: en COMILLAS tambien trato los apostrofes ')
aisla_grupos()

' seguido, busca llaves y parentesis
busca_llaves_parentesis_puntoycoma()

' elimina palabas clave engorrosas, como VOID, LOCAL, PRIVATE, PUBLIC, EXTERN, STATIC, etc
elimina_engorros()

' casos TYPEDEF/STRUCT y ENUM
trata_typedef_struct_enum()

' identifica si es una instruccion o una subrutina, y lo arregla como tal
instrucciones()

' trato de localizar variables que NO he cambiado, para ver si puedo arreglar (dentro de "{}")
busca_variables()

' FOR ... NEXT
trata_for() ' debe ir antes de "cambio_basicos" por que alteramos los "()" necesarios al buscar "FOR (;;)"

' cambios sencillos, como cambiar ' por " (caracter unitario) y corchetes por llaves
cambios_basicos()

' caracteres de operaciones tipo = & ! ^ | etc
operadores()

' IF ... THEN
trata_if()

' cambia [] por () (nota:debe ir tras TRATA_IF para no alterar resultados, sino, algunos THEN fallan)
cambia_corchetes()

' WHILE ... END
trata_while()

' SWITCH ... CASE
trata_case()

' quitamos los ":"
quita_dospuntos()

' lo ultimo, quitar el ; y cambiar por : donde sea necesario (lineas continuas)
cambia_puntoycoma()

'''''''''''''''
	ACABAR(tcar) ' tcar=fichero completo
'''''''''''''''



