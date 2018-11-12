*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : IWP093.PRG -> Manual
*-H Release			: 02
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_modo
PRIVATE all like w_*

** Salvado de estados anteriores

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu

** Apertura de archivos **

fieldvec='F93 - Documentaci๓n'

use &d0f93 in 0 alias f93 order tag d93desc && Guia del sistema

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

w_pfile='93'

w_prog=upper(prompt())

w_alias='F93'

w_title=db_name(w_alias)

sele (w_alias)

w_alta=.f.

set skip of menu _msysmenu .t.

do ssm998.mpr

=skipbar('Registro','Confirmar', .t.)
=skipbar('Registro','Elegir', .t.)


w_orde=1
w_tema=d93desc+'     '+d93docu

do iws093.spr

pop key
pop menu _msysmenu

=closall()

read when .f.

return

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Valid93
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()

if w_vari='W_TEMA'
	w_rec=left(w_tema,25)
	do cons93
endif

return w_retu

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Cons93
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

sele (w_alias)
=seek(w_rec,'f93')
if found()
*	=winword(alltrim(d93docu))	&& R.01 Mariano
	=winpdf(alltrim(d93docu))	&& R.01 Mariano
	_curobj=objnum(w_tema)	&& R.02 Mariano
endif
go top in &w_alias

return

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure xxCons93
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

sele (w_alias)
=seek(w_rec,'f93')
if found()

	keyboard '{enter}'
	keyboard '{enter}'

	modify general d93text nomodify

endif
go top in &w_alias

return

