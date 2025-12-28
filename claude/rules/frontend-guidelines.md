# Frontend Architecture Guidelines

## Separation of Concerns
- Separate presentation from business logic
- Keep leaf components presentational (no business logic)
- Business logic belongs in hooks, stores, or services

## Component Architecture
- Presentational components receive data via props
- Container/smart components handle state and side effects
- Avoid `useState` and `useEffect` in view-only components

## Styling Discipline
- Prefer design system tokens over arbitrary values
- Use predefined utility classes (e.g., `p-base`, `p-double`) over arbitrary spacing
- Maintain consistency with existing design patterns

## Code Quality
- Fix linter errors at root cause - avoid `eslint-disable` comments
- Prioritize simplest changes and code readability
