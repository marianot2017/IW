*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H Programa        : IWP097.PRG -> Guia del sistema
*-H Release			  : 02
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PARAMETERS w_modo
PRIVATE all like w_*

** Salvado de estados anteriores

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu

** Apertura de archivos **

fieldvec='F97 - Documentaci�n'

use &d0f97 in 0 alias f97 order tag d97desc && Guia del sistema

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

w_pfile='97'

w_prog=upper(prompt())

w_alias='F97'

w_title=db_name(w_alias)

sele (w_alias)

w_alta=.f.

set skip of menu _msysmenu .t.

do ssm998.mpr

=skipbar('Registro','Confirmar', .t.)
=skipbar('Registro','Elegir', .t.)


w_orde=1
w_tema=d97desc+'     '+d97docu

do iws097.spr

pop key
pop menu _msysmenu

=closall()

read when .f.

return

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure Valid97
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
w_retu=.t.
w_vari=varread()

if w_vari='W_TEMA'
	w_rec=left(w_tema,25)
	do cons97
endif

return w_retu

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure Cons97
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

sele (w_alias)
=seek(w_rec,'f97')
if found()
*	=winword(alltrim(d97docu))	&& R.01 Mariano
	=winpdf(alltrim(d97docu))	&& R.01 Mariano
	_curobj=objnum(w_tema)	&& R.02 Mariano
endif
go top in &w_alias

return

*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Procedure xxCons97
*-P 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

sele (w_alias)
=seek(w_rec,'f97')
if found()

	keyboard '{enter}'
	keyboard '{enter}'

	modify general d97text nomodify

endif
go top in &w_alias

return

