# Language-Specific Considerations

## TypeScript/JavaScript

- Prefer interfaces over types for object shapes
- Use `readonly` for immutability
- Avoid `any`, use `unknown` with type guards
- Use discriminated unions over enums
- Prefer async/await over raw promises

## Java

- Use records for DTOs (Java 14+)
- Prefer constructor injection with Lombok
- Use Optional for nullable returns
- Avoid `@Autowired` on fields
- Use streams for collection operations

## C#

- Use records for immutable DTOs
- Prefer primary constructors
- Use nullable reference types
- Avoid `async void`
- Use LINQ for collection operations

## Python

- Use dataclasses for simple DTOs
- Type hints required for public APIs
- Prefer composition over inheritance
- Use `Optional[Type]` for nullable
- Follow PEP 8 naming conventions
