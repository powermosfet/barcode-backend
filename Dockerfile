FROM archlinux:latest
RUN mkdir -p /opt/barcode-backend/
ARG BINARY_PATH
WORKDIR /opt/barcode-backend
RUN pacman -Syu --noconfirm
COPY "$BINARY_PATH" /opt/barcode-backend

ENV PORT=8080
ENV DB_HOST=postgres
ENV DB_PORT=5432
ENV DB_USER=john.doe
ENV DB_PW=secret

CMD ["/opt/barcode-backend/barcode-backend"]

