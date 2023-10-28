USE sql_project;

-- add random dates 

ALTER TABLE sales
ADD sale_date DATE;

UPDATE sales
SET sale_date = DATE_ADD('2021-01-01', INTERVAL FLOOR(RAND() * 365*3) DAY);

-- add price to products with $0 price
UPDATE products
SET Price = CASE
WHEN Price = 0 THEN ROUND(RAND() * 100) + 1 
ELSE Price
END;

-- Calculate customer lifetime
CREATE VIEW customer_lifetime AS
SELECT CustomerID,
		CONCAT(
		TIMESTAMPDIFF(YEAR,MIN(sale_date), MAX(sale_date)), ' years and ',
        TIMESTAMPDIFF(MONTH,MIN(sale_date), MAX(sale_date)) - 
        TIMESTAMPDIFF(YEAR,MIN(sale_date), MAX(sale_date)) * 12, ' months '
        )
        AS customer_lifetime
FROM sales 
GROUP BY CustomerID;

-- arcived and active orders 

SELECT *,
		CASE
        WHEN YEAR(sale_date) = YEAR(NOW()) THEN 'Active'
        WHEN YEAR(sale_date) = YEAR(NOW()) - 1 THEN 'Last Year'
        ELSE 'Archived'
        END as date_status
FROM sales;


-- Find the revenue generated from each product

SELECT ProductID,
		Name,
        Price,
        SUM(Quantity) AS total_sales,
        ROUND(SUM(Quantity) * Price, 2 ) AS Revenue
FROM sales s
JOIN products p USING(productID)
GROUP BY ProductID , Price, Name, Price
ORDER BY Revenue DESC
LIMIT 10;

-- employees that bring in the highest revenue

CREATE VIEW best_employees AS 
SELECT SalesPersonID,
        CONCAT(FirstName,' ', MiddleInitial,' ',LastName) AS SalesmanName,
        ROUND(SUM(Quantity) * Price, 2 ) AS revenue_brought,
        SUM(Quantity) AS units_sold
FROM sales s 
JOIN employees e ON s.SalesPersonID = e.employeeID
JOIN products USING (productID)
GROUP BY SalesPersonID, FirstName, MiddleInitial, LastName, Price
ORDER BY revenue_brought DESC;

-- Point system for employees (.3*units sold + .7 revenue)/10000000 and categorize

SELECT *,
       CEILING((revenue_brought * 0.7 + 0.3 * units_sold) / 10000) AS points,
       CASE 
			WHEN (SELECT points ) > 2000 THEN 'Elite'
            WHEN (SELECT points) > 1000 THEN 'Top'
            WHEN (SELECT points) > 700 THEN 'High'
            WHEN (SELECT points )> 400 THEN 'Consistent' 
            ELSE 'Emerging'
            END AS Ranking
	
FROM best_employees;

-- Products with higher price than the average using correlated subqueries

SELECT *
FROM products p
WHERE Price > 
				(SELECT AVG(Price)
                FROM products 
                WHERE Price = p.Price);
                

 









