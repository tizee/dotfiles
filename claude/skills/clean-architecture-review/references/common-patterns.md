# Common Patterns & Solutions

## Pattern: Dependency Rule Violation

**Problem**:
```typescript
// ❌ Domain layer imports from Infrastructure
import { UserRepository } from '../infrastructure/UserRepository';

export class CreateUserUseCase {
  constructor(private repo: UserRepository) {} // Concrete dependency
}
```

**Solution**:
```typescript
// ✅ Domain layer defines interface
export interface IUserRepository {
  save(user: User): Promise<void>;
  findById(id: string): Promise<User | null>;
}

export class CreateUserUseCase {
  constructor(private repo: IUserRepository) {} // Abstract dependency
}

// Infrastructure implements interface
export class UserRepository implements IUserRepository {
  // Implementation details here
}
```

---

## Pattern: God Class

**Problem**:
```typescript
// ❌ Class does too much
export class OrderManager {
  createOrder() {}
  cancelOrder() {}
  processPayment() {}    // Should be in PaymentService
  sendEmail() {}         // Should be in EmailService
  updateInventory() {}   // Should be in InventoryService
  generateReport() {}  // Should be in ReportService
}
```

**Solution**:
```typescript
// ✅ Each class has single responsibility
export class OrderService {
  constructor(
    private paymentService: PaymentService,
    private emailService: EmailService,
    private inventoryService: InventoryService
  ) {}
  
  async createOrder(orderData: OrderData) {
    // Orchestrate, don't implement
    await this.inventoryService.reserve(orderData.items);
    const payment = await this.paymentService.charge(orderData.payment);
    await this.emailService.sendConfirmation(orderData.customer);
    // ...
  }
}
```

---

## Pattern: Feature Envy

**Problem**:
```typescript
// ❌ Order uses Customer's data extensively
export class Order {
  calculateDiscount(customer: Customer): number {
    if (customer.isVIP() && customer.getYearsAsCustomer() > 5) {
      return this.total * 0.2;
    }
    return 0;
  }
}
```

**Solution**:
```typescript
// ✅ Move method to the class it envies
export class Customer {
  calculateDiscount(orderTotal: number): number {
    if (this.isVIP() && this.yearsAsCustomer > 5) {
      return orderTotal * 0.2;
    }
    return 0;
  }
}

export class Order {
  calculateDiscount(customer: Customer): number {
    return customer.calculateDiscount(this.total);
  }
}
```

---

# Decision Tables

## When to Extract Method vs Extract Class

| Scenario | Action | Example |
|----------|--------|---------|
| Method > 20 lines, but uses only local data | Extract Method | Complex calculation in service |
| Method > 20 lines, uses different data groups | Extract Class | Order processing with payment + shipping logic |
| Class > 200 lines, multiple responsibilities | Extract Class | UserManager with auth + profile + preferences |
| Related methods share subset of fields | Extract Class | Reporting methods using date range |

## When to Use Inheritance vs Composition

| Use Inheritance When | Use Composition When |
|---------------------|---------------------|
| True "is-a" relationship | "has-a" or "uses-a" relationship |
| Subclass is always a valid substitute | Behavior varies at runtime |
| Code reuse of interface + implementation | Only need to reuse implementation |
| Small, stable base class | Multiple sources of behavior |

## Repository vs Service vs Use Case

| Component | Responsibility | Example |
|-----------|---------------|---------|
| **Repository** | Data access, persistence | `userRepo.findById()`, `orderRepo.save()` |
| **Domain Service** | Business logic spanning aggregates | `pricingService.calculateTotal()` |
| **Application Service** | Orchestrate use case flow | `createOrderUseCase.execute()` |
| **Controller** | HTTP handling, input validation | `orderController.create()` |
