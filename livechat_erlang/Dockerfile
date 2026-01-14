FROM erlang:26-alpine
WORKDIR /app

# Install dependencies
RUN apk add --no-cache git curl

# Copy rebar3 dependencies config
COPY rebar.config .

# Download and fetch dependencies
RUN rebar3 get-deps
RUN rebar3 compile

# Copy application source
COPY . .

# Compile the application
RUN rebar3 compile

EXPOSE 4001

# Run the application
CMD ["rebar3", "shell", "--sname", "livechat@localhost"]
