*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H Programa        : IWP095 -> Documentaci�n Complementaria
*-H Release			  : 02
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H Fecha       Programmer      Comentarios
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PARAMETERS w_modo

PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear      &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F95 - Documentaci�n'
use (d0f95) in 0 alias f95 order tag d95desc &&(DOC.COMPLEM.)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*) Varibles internas-----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_pfile='95'
w_alias='F95'
w_title=db_name(w_alias)

do ssm998.mpr
=skipbar('Registro','Confirmar', .t.)
=skipbar('Registro','Elegir', .t.)

sele (w_alias)

w_orde=1
w_tema=d95desc+'     '+d95docu

do iws095.spr

pop key
pop menu _msysmenu
=closall()
read when .f.
return

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure Valid95
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
w_retu=.t.
w_vari=varread()

if w_vari='W_TEMA'
	w_rec=left(w_tema,25)
	do cons95
endif

return w_retu

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure Cons95
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

sele (w_alias)
=seek(w_rec,'f95')
if found()
*	=winword(alltrim(d95docu))	&& R.01 Mariano
	=winpdf(alltrim(d95docu))	&& R.01 Mariano
	_curobj=objnum(w_tema)	&& R.02 Mariano
endif
go top in f95
return

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure xxCons95
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

sele (w_alias)
=seek(w_rec,'f95')
if found()

	keyboard '{enter}'
	keyboard '{enter}'

	modify general d95text

endif
go top in f95
return
