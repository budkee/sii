# Lisp on Docker

Piensando en no utilizar las configuraciones de mi ordenador para ejecutar la lenguagen LISP, fue criado un **Dockerfile** para configuración de un ambiente de desenvolvimiento com **SBCL** e **Quicklisp**. 

Para criar e ejecutar un contenedor Docker rodando **Lisp** usted tiene dos opciones:

- [Lisp on Container](#lisp-con-contenedor)
- [Lisp on Docker Compose](#lisp-con-docker-compose)

Para que pueda ejecutar el código y testar los ejemplos en la carpeta `scripts`, en la raíz del proyecto, puedes optar por hacer de dos maneras:

1. [Ejecutar un contenedor especificando el archivo.](#-3-ejecutar-un-script-lisp-en-el-contenedor)

2. [Ejecutar una composición de dejála activa.](#)

```bash
docker compose up -d
```

Y después ejecute el script deseado por medio de:

```bash
docker compose exec lisp sbcl --script "/scripts/script.lisp"
```

---

## Lisp con Contenedor

### 📌 1. Crear el `Dockerfile`

Cree un archivo llamado `Dockerfile` con el siguiente contenido:

```dockerfile
# Usar imagen base de Debian
FROM debian:latest

# Actualizar paquetes e instalar dependencias
RUN apt-get update && apt-get install -y \
    sbcl \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Descargar e instalar Quicklisp
RUN curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
         --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
         --eval '(ql:add-to-init-file)' \
         --quit && \
    rm /tmp/ql.lisp

# Instalar Quicklisp-Slime-Helper
RUN sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

# Definir el directorio de trabajo
WORKDIR /scripts

```

---

### 📌 2. Crear y ejecutar el contenedor

En el mismo directorio del `Dockerfile`, ejecute:

1. **Construir la imagen Docker:**

   ```sh
   docker build -t sbcl-lisp .
   ```

2. **Ejecutar un contenedor interactivo con el REPL de Lisp:**
   ```sh
   docker run -it --name debian-lisp --rm sbcl-lisp
   ```

Esto abrirá un **REPL de SBCL** dentro del contenedor y lo destruirá poco después.

---

### 📌 3. Ejecutar un script Lisp en el contenedor

Para ejecutar un **script Lisp** (por ejemplo, `script.lisp`) dentro del contenedor:

```sh
docker run --rm -it -v "$(pwd)/scripts/script.lisp:/scripts/script.lisp" --name debian-lisp sbcl-lisp sbcl --script /scripts/script.lisp 
```

Esto monta el archivo en el contenedor y lo ejecuta.

---

## Lisp con Docker Compose

Para montar un volumen para la carpeta `scripts/` usando **Docker Compose**:

### 📌 1. Estructura del proyecto

```
/lisp
│── docker-compose.yml
│── Dockerfile
└── scripts/
    ├── script.lisp
    ├── otro_script.lisp
```

La carpeta `scripts/` contiene los archivos Lisp que se accederán desde el contenedor.

---

### 📌 2. Crear el `docker-compose.yml`

Cree el archivo `docker-compose.yml` en la raíz del proyecto, tenendo en cuenta el [**Dockerfile** anterior](#-1-crear-el-dockerfile):

```yaml
services:
  sii:
    container_name: lisp
    build: .
    volumes:
      - ./scripts:/scripts
    working_dir: /scripts
    # Mantém o container aberto
    stdin_open: true 
    # Habilita um terminal interativo
    tty: true 
    command: ["tail", "-f", "/dev/null"]
```

Este archivo:

- **Construye la imagen** usando el `Dockerfile`.
- **Monta la carpeta `scripts/`** del host dentro del contenedor en `/scripts`.
- **Define el directorio de trabajo** como `/scripts`, haciendo accesibles los archivos Lisp.
- **Inicia SBCL** automáticamente.

---

### 📌 3. Ejecutar el contenedor

1️⃣ **Construir e iniciar el contenedor:**

```sh
docker compose up -d
```

Esto crea el contenedor y lo mantiene en segundo plano.

2️⃣ **Acceder al REPL de SBCL dentro del contenedor:**

```sh
docker compose exec sii sbcl
```

Para salir digite:

```lisp
(sb-ext:quit)
```

3️⃣ **Ejecutar un script Lisp dentro del contenedor:**

```sh
docker compose exec sii sbcl --script "/scripts/script.lisp"
```

o

```sh
docker compose exec sii sbcl --noinform --load "/scripts/script.lisp"
```

4️⃣ **Parar y eliminar los contenedores:**

```sh
docker compose down
```

## Diferencias al utilizar `--script` o `--noinform --load`

| Modo                | Sale del SBCL | Uso recomendado                      |
| ------------------- | ------------- | ------------------------------------ |
| `--script`          | ✅ Sí         | Ejecutar y salir automáticamente     |
| `--noinform --load` | ❌ No         | Cargar código y continuar en el REPL |
