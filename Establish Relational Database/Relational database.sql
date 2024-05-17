#MGMT 58200 MoD Project SQL Script - Group 2
####1 Create Order table
#####Step 1. Create temperory order table
create table Orders_staging(
OrderID varchar(255),
CustomerID varchar(255),
PaymentID varchar(255),
OrderGeneratedPoint varchar(255),
OrderUsedPoint varchar(255),
CouponID varchar(255),
DeliveryID varchar(255),
OrderDateTime varchar(255),
CancelDateTime varchar(255),
DeliveryStatus varchar(255),
TotalAmount float
);
######After this step load data from csv into this
#####Step 2. Create main order table
create table Orders(
OrderID varchar(255) primary key,
CustomerID int,
PaymentID varchar(255),
OrderGeneratedPoint int,
OrderUsedPoint int,
CouponID varchar(255),
DeliveryID varchar(255),
OrderDateTime datetime,
CancelDateTime datetime,
DeliveryStatus varchar(255),
TotalAmount float
);
#####Step 3. Modify data from temp table and load to main table
insert into Orders
select OrderID,
CustomerID,
PaymentID,
cast(nullif(OrderGeneratedPoint, '') as unsigned) as OrderGeneratedPoint,
cast(nullif(OrderUsedPoint, '') as unsigned) as OrderUsedPoint,
CouponID,
DeliveryID,
cast(OrderDateTime as datetime) as OrderDateTime,
str_to_date(nullif(CancelDateTime, ''), '%Y/%m/%d %H:%i') as CancelDateTime,
DeliveryStatus,
TotalAmount
from Orders_staging;

######################################################
####2 Create Order_Composition table
drop table sc.Order_Composition;
create table sc.Order_Composition(
    order_id varchar(255),
    product_id int,
    quantity int,
    promotion_id int default null,
    coupon_id varchar(255) default null,
    primary key(order_id, product_id)
);

######################################################
####3 Create Coupon table
create table Coupon (
	CouponID int primary key,
    CouponName varchar(255),
    CouponAmount float
);

######################################################
####4 Create Customer table
create table sc.Customer(
    member_id int primary key,
    member_name varchar(255),
    member_city varchar(255),
    member_phone_number varchar(255)
);

######################################################
####5 Create Promotion table
create table sc.Promotion(
    promotion_id int primary key,
    promotion_name varchar(255),
    promotion_amount int
);

######################################################
####6 Create Delivery table
create table sc.Delivery(
    delivery_id varchar(250) primary key,
	delivery_company varchar(50),
	delivery_method varchar(50),
	delivery_fee int,
    shipping_status char(100),
    customer_receive_time datetime,
    delivery_city varchar(30) 
);

######################################################
####7 Create Product table
create table sc.Products(
    product_id int primary key,
    product_name nvarchar(255),
    product_type nvarchar(255),
    product_category nvarchar(255),
    product_price float
);

######################################################
####8 Create Payment table
#####Step 1. Create raw payment table by importing the payment data
Select * from payment_raw
limit 10;


#####Step 2. Transfer data to final table after cleaning 
Create Table payment_new(

Select 
PaymentID as PaymentID,
PaymentMethod as PaymentMethod,
PaymentStatus as PaymentStatus,
`Payment Date and Time` as PaymentDateTime
from  payment_raw
where PaymentID !=  ''
group by PaymentID, PaymentMethod, PaymentStatus , PaymentDateTime
);

############################################################################################################
###Q1 What is the best-selling category/item by amount sold? 
select distinct(a.product_category) as "Product Category", sum(c.TotalAmount) as "Total Sales"
from sc.products as a
join sc.order_composition as b
join sc.orders as c
on a.product_ID = b.product_id and b.order_id = c.OrderID
where (c.OrderStatus = "Closed") and (a.product_category is not null or a.product_category <> '')
group by a.product_category
order by sum(c.TotalAmount) desc
limit 2
offset 1; #There are blanks in product category since the business is new, so excluding the blank row


######################################################
###Q2 What is the worst-selling category/item by amount? 
select distinct(a.product_category) as "Product Category", sum(c.TotalAmount) as "Total Sales"
from sc.products as a
join sc.order_composition as b
join sc.orders as c
on a.product_ID = b.product_id and b.order_id = c.OrderID
where (c.OrderStatus = "Closed") and (a.product_category is not null or a.product_category <> '')
group by a.product_category
order by sum(c.TotalAmount) asc
limit 2;



######################################################
###Q3 What is the peak month for sales? 
select distinct monthname(a.OrderDateTime) as "Month", year(a.OrderDateTime) as "Year", sum(a.TotalAmount) as "Total Sales"
from sc.orders as a
where a.OrderStatus = "Closed"
group by monthname(a.OrderDateTime), year(a.OrderDateTime)
order by sum(a.TotalAmount) desc
limit 5;


######################################################
###Q4 What percentage of payments are actually completed? 

SET @TotalOrderCount = (SELECT COUNT(DISTINCT OrderID) FROM orders);

SELECT OrderStatus, (COUNT(DISTINCT OrderID))*100/@TotalOrderCount AS OrderPercentage
FROM orders
GROUP BY OrderStatus;



######################################################
###Q5 What is the average order amount (monetary) per customer?

SELECT SUM(TotalAmount)/COUNT(DISTINCT CustomerID) AS AverageRevenuePerCustomer
FROM orders
WHERE OrderStatus = "Closed";



######################################################
###Q6 What is the average order amount (items) per customer?

SELECT COUNT(B.ProductID)/COUNT(DISTINCT A.CustomerID) AS AverageProductsPerCustomer
FROM orders AS A
LEFT JOIN order_composition AS B ON A.OrderID = B.OrderID
WHERE OrderStatus = "Closed";



######################################################
###Q7 Get the most loyal customer list

# Get Latest Customers for last 3 months
WITH RecentCustomers AS (
SELECT DISTINCT CustomerID FROM orders
WHERE OrderStatus = "Closed"
AND OrderDateTime > '2022-09-30') #last customers from last 3 months of data

# Get Total Customer Revenue
, CustomerRevenue AS (
SELECT CustomerID, SUM(TotalAmount) AS TotalAmount
FROM orders
WHERE OrderStatus = "Closed"
GROUP BY CustomerID)

# Customers ranked by revenue
, CustomerRevRank AS (
SELECT *, ROW_NUMBER() OVER(ORDER BY TotalAmount DESC) AS CustRank
FROM CustomerRevenue)

# Top Performing Customers
, TopRevCustomers AS (
SELECT CustomerID FROM CustomerRevRank WHERE CustRank <= 30)

# Get Total Customer Order Count
, CustomerOrderCount AS (
SELECT CustomerID, COUNT(DISTINCT OrderID) AS OrderCount
FROM orders
WHERE OrderStatus = "Closed"
GROUP BY CustomerID)

# Customers ranked by order count
, CustomerOrderRank AS (
SELECT *, ROW_NUMBER() OVER(ORDER BY OrderCount DESC) AS CustRank
FROM CustomerOrderCount)

# Customers with most orders
, TopOrderCustomers AS (
SELECT CustomerID FROM CustomerOrderRank WHERE CustRank <= 30)

# Get the Loyal Customers
, LoyalCustomers AS (
SELECT A.CustomerID 
FROM RecentCustomers AS A
INNER JOIN TopRevCustomers AS B ON A.CustomerID = B.CustomerID
INNER JOIN TopOrderCustomers AS C ON A.CustomerID = C.CustomerID)

# Loyal Customer Info
SELECT A.CustomerID, 
IF(CustomerName = '', "Unknown", CustomerName) AS CustomerName,
IF(CustomerCity = '', "Unknown", CustomerCity) AS CustomerCity,
IF(CustomerPhoneNumber = '', "Unknown", CustomerPhoneNumber) AS CustomerPhoneNumber
FROM LoyalCustomers AS A
INNER JOIN Customer AS B ON A.CustomerID = B.CustomerID;



######################################################
###Q8 Which promotions result in higher orders?
select sum(quantity) as order_amount,PromotionName from order_composition as t1
left join promotion as t2
on t1.PromotionID = t2.PromotionID
group by PromotionName
order by order_amount desc;


######################################################
###Q9 Which city has the most customers?
select count(CustomerID) as customer_amount,member_city from orders as t1
left join customer as t2
on t1.CustomerID = t2.member_id
group by member_city
order by customer_amount desc;



######################################################
###Q10 What is the percentage share of each delivery method?
select DeliveryMethod,count(DeliveryMethod) as total,
(count(DeliveryMethod)*100)/(select count(*) from delivery) as percentage
from delivery
group by DeliveryMethod
order by percentage desc;


