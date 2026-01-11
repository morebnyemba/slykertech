FROM erlang:26-alpine
WORKDIR /app
RUN apk add --no-cache git
COPY . .
EXPOSE 4001
CMD ["erl", "-noshell"]
