*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H Programa        : IWP900C.PRG -> C.U.I.T.
*)-H
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.

*) Datos del proceso-----------------------------------------------------------

w_pfile='900c'
w_title='C.U.I.T.'

w_btnok=0

w_base=''
w_cuit=''

*)-----------------------------------------------------------------------------

do iws900c.spr

pop key
pop menu _msysmenu
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
FUNC valid900c
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
w_cuit=w_base+'-'+dicuit(w_base)
show get w_cuit
return
