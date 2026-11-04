# Stage 1: Build the uberjar
FROM clojure:temurin-21-tools-deps-alpine AS builder

WORKDIR /build

# Cache project and build dependencies
COPY deps.edn .
RUN clojure -P -T:build

# Copy build script and application source code
COPY build.clj .
COPY src ./src

# Compile and package into standalone uberjar
RUN clojure -T:build uber

# Stage 2: Minimal runtime image
FROM eclipse-temurin:21-jre-alpine

WORKDIR /app

# Install dumb-init for proper signal forwarding and zombie process reaping
RUN apk add --no-cache dumb-init

# Copy uberjar from builder stage
COPY --from=builder /build/target/server.jar ./app.jar

# Expose mDNS UDP port (5353), HTTP configuration server TCP port (3000), and debug port (5005)
EXPOSE 5353/udp
EXPOSE 3000/tcp
EXPOSE 5005/tcp

# Ensure Java prefers IPv4 stack for multicast networking (224.0.0.251)
ENV JAVA_TOOL_OPTIONS="-Djava.net.preferIPv4Stack=true"

# Run Java directly under dumb-init without a shell wrapper
ENTRYPOINT ["dumb-init", "--", "java", "-jar", "app.jar"]
