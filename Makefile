## Individual intervention

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

## individual_intervention.pdf: individual_intervention.tex

Sources += individual_intervention.tex

######################################################################

## Figures are tikz and not pipelined yet 2021 May 04 (Tue)

autowrapR = defined

%.tex: %.Rout ;

## scenarios.pdf: scenarios.R

bgsim.Rout: bgsim.R sir-semi.rda
	$(pipeR)

scenarios.Rout: scenarios.R bgsim.rda
	$(pipeR)

######################################################################

## Redeveloping in Phila ☹

figure_beta_gamma.Rout: figure_beta_gamma.R
	$(pipeR)

######################################################################

### Makestuff

## Sources += $(wildcard *.mk)
## include $(wildcard *.mk)

Sources += Makefile
Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
