# Usar imagem base do SBCL
FROM debian:latest

# Instalar dependências
RUN apt-get update && apt-get install -y \
    sbcl \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Baixar e instalar Quicklisp
RUN curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
         --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
         --eval '(ql:add-to-init-file)' \
         --quit && \
    rm /tmp/ql.lisp

# Instalar Quicklisp-Slime-Helper
RUN sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

# Definir diretório de trabalho
WORKDIR /scripts

# Comando padrão: iniciar um REPL do SBCL
# CMD ["sbcl"]
