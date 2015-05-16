FROM buildpack-deps:jessie
RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.2.11-x86-64-linux-binary.tar.bz2 &&  tar xjf sbcl-1.2.11-x86-64-linux-binary.tar.bz2 && cd sbcl-1.2.11-x86-64-linux && sh install.sh
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path #P"./lisp")' && rm quicklisp.lisp
RUN sbcl --non-interactive --load lisp/setup.lisp --eval '(ql:quickload "buildapp")' --eval '(buildapp:build-buildapp)'
ADD . /aleasrc
RUN ./buildapp --load lisp/setup.lisp --asdf-path aleasrc/ --eval '(ql:quickload "alea")' --eval '(defun main (argv) (alea:launch argv))' --entry main --output aleabot
# docker run --rm imagename tar cz aleabot > aleabot.tgz
CMD "./aleabot"
