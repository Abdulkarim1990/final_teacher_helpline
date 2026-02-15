USE oqhm7raqbkfdrtqz;

-- =============================================================================
-- Helper: safe ADD COLUMN (skips if column already exists)
-- =============================================================================
DROP PROCEDURE IF EXISTS safe_add_column;
DELIMITER //
CREATE PROCEDURE safe_add_column(
  IN tbl VARCHAR(64),
  IN col VARCHAR(64),
  IN col_def VARCHAR(500)
)
BEGIN
  SET @col_exists = (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = DATABASE() AND table_name = tbl AND column_name = col
  );
  IF @col_exists = 0 THEN
    SET @ddl = CONCAT('ALTER TABLE `', tbl, '` ADD COLUMN `', col, '` ', col_def);
    PREPARE stmt FROM @ddl;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;
  END IF;
END//
DELIMITER ;

-- =============================================================================
-- Helper: safe CREATE INDEX (skips if index already exists)
-- =============================================================================
DROP PROCEDURE IF EXISTS safe_create_index;
DELIMITER //
CREATE PROCEDURE safe_create_index(
  IN idx_name VARCHAR(64),
  IN tbl VARCHAR(64),
  IN idx_columns VARCHAR(500)
)
BEGIN
  SET @idx_exists = (
    SELECT COUNT(*) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = tbl AND index_name = idx_name
    LIMIT 1
  );
  IF @idx_exists = 0 THEN
    SET @ddl = CONCAT('CREATE INDEX `', idx_name, '` ON `', tbl, '` (', idx_columns, ')');
    PREPARE stmt FROM @ddl;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;
  END IF;
END//
DELIMITER ;


-- #############################################################################
-- SECTION 1: REFERENCE TABLES
-- #############################################################################

-- Ghana's 16 regions
CREATE TABLE IF NOT EXISTS regions (
  region_id   INT AUTO_INCREMENT PRIMARY KEY,
  region_name VARCHAR(100) NOT NULL UNIQUE,
  region_code VARCHAR(10)  NOT NULL UNIQUE,
  is_active   BOOLEAN DEFAULT TRUE,
  created_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- User roles and access levels
CREATE TABLE IF NOT EXISTS users (
  user_id       INT AUTO_INCREMENT PRIMARY KEY,
  full_name     VARCHAR(150) NOT NULL,
  email         VARCHAR(200) UNIQUE NOT NULL,
  phone         VARCHAR(20),
  role          ENUM('Regional Agent','Regional Supervisor','National Resolver','National Admin') NOT NULL,
  region_id     INT NULL,  -- NULL for national roles
  is_active     BOOLEAN DEFAULT TRUE,
  password_hash VARCHAR(255) NOT NULL,
  last_login    TIMESTAMP NULL,
  created_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  CONSTRAINT fk_users_region FOREIGN KEY (region_id) REFERENCES regions(region_id)
);

-- Issue categories based on GES concept note
CREATE TABLE IF NOT EXISTS issue_categories (
  category_id              INT AUTO_INCREMENT PRIMARY KEY,
  category_code            VARCHAR(20)  NOT NULL UNIQUE,
  category_name            VARCHAR(120) NOT NULL UNIQUE,
  default_national_role    VARCHAR(100) NOT NULL,
  first_response_sla_hours INT DEFAULT 24,
  resolution_sla_hours     INT DEFAULT 240,  -- 10 business days
  escalation_trigger_hours INT DEFAULT 72,
  description              TEXT,
  is_active                BOOLEAN DEFAULT TRUE,
  created_at               TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Subcategories for detailed classification
CREATE TABLE IF NOT EXISTS issue_subcategories (
  subcategory_id   INT AUTO_INCREMENT PRIMARY KEY,
  category_id      INT NOT NULL,
  subcategory_code VARCHAR(20)  NOT NULL,
  subcategory_name VARCHAR(150) NOT NULL,
  is_active        BOOLEAN DEFAULT TRUE,
  created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_subcategories_category FOREIGN KEY (category_id) REFERENCES issue_categories(category_id),
  UNIQUE KEY unique_subcat_code (category_id, subcategory_code)
);

-- Communication channels
CREATE TABLE IF NOT EXISTS channels (
  channel_id   INT AUTO_INCREMENT PRIMARY KEY,
  channel_name VARCHAR(50) NOT NULL UNIQUE,
  is_active    BOOLEAN DEFAULT TRUE
);


-- #############################################################################
-- SECTION 2: MAIN OPERATIONAL TABLES
-- #############################################################################

-- Tickets (cases) with comprehensive tracking
CREATE TABLE IF NOT EXISTS tickets (
  ticket_id              BIGINT AUTO_INCREMENT PRIMARY KEY,
  case_code              VARCHAR(30) UNIQUE DEFAULT NULL,
  created_at             TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by_user_id     INT NULL,
  region_id              INT NOT NULL,
  channel_id             INT DEFAULT 1,

  -- Teacher / caller information
  teacher_name           VARCHAR(120),
  teacher_phone          VARCHAR(40) NULL,
  teacher_staff_id       VARCHAR(40),
  school_name            VARCHAR(200),
  district               VARCHAR(120),

  -- Issue classification
  category_id            INT NOT NULL,
  subcategory_id         INT NULL,
  priority               ENUM('Low','Medium','High','Urgent') NOT NULL DEFAULT 'Medium',
  status                 ENUM('New','In Progress','Waiting on Teacher','Escalated','Resolved','Closed') NOT NULL DEFAULT 'New',
  summary                TEXT NOT NULL,
  description            TEXT NULL,

  -- Entry mode: 'full' for detailed cases, 'quick' for routine resolved cases
  entry_mode             ENUM('full','quick') NOT NULL DEFAULT 'full',
  quick_outcome          VARCHAR(50) NULL,

  -- Workflow timestamps
  first_response_at      TIMESTAMP NULL,
  escalated_at           TIMESTAMP NULL,
  resolved_at            TIMESTAMP NULL,
  closed_at              TIMESTAMP NULL,

  -- SLA tracking
  first_response_due_at  TIMESTAMP NULL,
  resolution_due_at      TIMESTAMP NULL,

  -- Assignments
  assigned_region_user_id   INT NULL,
  assigned_national_user_id INT NULL,

  -- Additional fields
  risk_level             ENUM('Low','Medium','High') NULL,
  is_formal_complaint    BOOLEAN DEFAULT FALSE,
  consent_to_contact     BOOLEAN DEFAULT TRUE,
  satisfaction_rating    TINYINT NULL,
  satisfaction_feedback  TEXT NULL,
  closure_reason         VARCHAR(200) NULL,
  escalation_reason      TEXT NULL,
  next_follow_up_date    DATE NULL,

  updated_at             TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

  CONSTRAINT fk_tickets_creator            FOREIGN KEY (created_by_user_id)     REFERENCES users(user_id),
  CONSTRAINT fk_tickets_region             FOREIGN KEY (region_id)              REFERENCES regions(region_id),
  CONSTRAINT fk_tickets_channel            FOREIGN KEY (channel_id)             REFERENCES channels(channel_id),
  CONSTRAINT fk_tickets_category           FOREIGN KEY (category_id)            REFERENCES issue_categories(category_id),
  CONSTRAINT fk_tickets_subcategory        FOREIGN KEY (subcategory_id)         REFERENCES issue_subcategories(subcategory_id),
  CONSTRAINT fk_tickets_regional_assignee  FOREIGN KEY (assigned_region_user_id)  REFERENCES users(user_id),
  CONSTRAINT fk_tickets_national_assignee  FOREIGN KEY (assigned_national_user_id) REFERENCES users(user_id),
  CONSTRAINT chk_satisfaction_rating       CHECK (satisfaction_rating BETWEEN 1 AND 5)
);

-- For existing databases: ensure columns added in later migrations exist
CALL safe_add_column('tickets', 'entry_mode',          "ENUM('full','quick') NOT NULL DEFAULT 'full'");
CALL safe_add_column('tickets', 'quick_outcome',       "VARCHAR(50) NULL");
CALL safe_add_column('tickets', 'escalation_reason',   "TEXT NULL");
CALL safe_add_column('tickets', 'next_follow_up_date', "DATE NULL");
CALL safe_add_column('tickets', 'satisfaction_rating',  "TINYINT NULL");
CALL safe_add_column('tickets', 'satisfaction_feedback', "TEXT NULL");

-- Comprehensive ticket actions log
CREATE TABLE IF NOT EXISTS ticket_actions (
  action_id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id         BIGINT NOT NULL,
  action_at         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  action_by_user_id INT NULL,
  action_type       ENUM('create','note','status_change','escalation','reassignment','attachment','first_response','resolution','closure') NOT NULL,
  action_text       TEXT,
  old_status        VARCHAR(50) NULL,
  new_status        VARCHAR(50) NULL,
  old_assignee_id   INT NULL,
  new_assignee_id   INT NULL,
  is_internal       BOOLEAN DEFAULT FALSE,

  CONSTRAINT fk_actions_ticket FOREIGN KEY (ticket_id)         REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_actions_user   FOREIGN KEY (action_by_user_id) REFERENCES users(user_id)
);

-- Escalation tracking
CREATE TABLE IF NOT EXISTS escalations (
  escalation_id     BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id         BIGINT NOT NULL,
  escalated_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  escalated_by_user_id INT NOT NULL,
  escalated_to_user_id INT NOT NULL,
  escalation_reason TEXT NOT NULL,
  escalation_method ENUM('Email','WhatsApp','Phone','System') DEFAULT 'System',
  escalation_status ENUM('Pending','In Review','Resolved','Rejected') DEFAULT 'Pending',
  resolved_at       TIMESTAMP NULL,
  resolution_notes  TEXT NULL,

  CONSTRAINT fk_escalations_ticket  FOREIGN KEY (ticket_id)            REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_escalations_by_user FOREIGN KEY (escalated_by_user_id) REFERENCES users(user_id),
  CONSTRAINT fk_escalations_to_user FOREIGN KEY (escalated_to_user_id) REFERENCES users(user_id)
);

-- File attachments
CREATE TABLE IF NOT EXISTS attachments (
  attachment_id      BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id          BIGINT NOT NULL,
  filename           VARCHAR(255) NOT NULL,
  file_path          VARCHAR(500) NOT NULL,
  file_size          INT NOT NULL,
  mime_type          VARCHAR(100),
  uploaded_at        TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  uploaded_by_user_id INT NOT NULL,

  CONSTRAINT fk_attachments_ticket FOREIGN KEY (ticket_id)          REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_attachments_user   FOREIGN KEY (uploaded_by_user_id) REFERENCES users(user_id)
);

-- Follow-ups table for scheduled reminders
CREATE TABLE IF NOT EXISTS follow_ups (
  follow_up_id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id           BIGINT NOT NULL,
  follow_up_date      DATE NOT NULL,
  follow_up_notes     TEXT,
  status              ENUM('Pending','Completed','Overdue','Cancelled') DEFAULT 'Pending',
  created_by_user_id  INT NOT NULL,
  created_at          TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  completed_at        TIMESTAMP NULL,
  completed_by_user_id INT NULL,

  CONSTRAINT fk_followups_ticket       FOREIGN KEY (ticket_id)           REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_followups_created_by   FOREIGN KEY (created_by_user_id)  REFERENCES users(user_id),
  CONSTRAINT fk_followups_completed_by FOREIGN KEY (completed_by_user_id) REFERENCES users(user_id)
);

-- Case templates for pre-defined responses
CREATE TABLE IF NOT EXISTS case_templates (
  template_id       INT AUTO_INCREMENT PRIMARY KEY,
  template_name     VARCHAR(150) NOT NULL,
  template_category VARCHAR(50)  NOT NULL,
  template_subject  VARCHAR(200) NOT NULL,
  template_body     TEXT NOT NULL,
  is_active         BOOLEAN DEFAULT TRUE,
  created_at        TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at        TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);


-- #############################################################################
-- SECTION 3: AUTHENTICATION & ACCESS CONTROL
-- #############################################################################

-- Login allowlist
CREATE TABLE IF NOT EXISTS login_allowlist (
  allowlist_id INT AUTO_INCREMENT PRIMARY KEY,
  email        VARCHAR(200) NOT NULL UNIQUE,
  is_active    BOOLEAN DEFAULT TRUE,
  added_by     VARCHAR(200) NULL,
  added_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  notes        TEXT NULL
);

-- Login audit trail
CREATE TABLE IF NOT EXISTS login_audit (
  audit_id     BIGINT AUTO_INCREMENT PRIMARY KEY,
  email        VARCHAR(200) NOT NULL,
  user_id      INT NULL,
  success      BOOLEAN NOT NULL DEFAULT FALSE,
  reason       VARCHAR(200) NULL,
  ip_address   VARCHAR(45) NULL,
  user_agent   TEXT NULL,
  attempted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_audit_user FOREIGN KEY (user_id) REFERENCES users(user_id)
);


-- #############################################################################
-- SECTION 4: INDEXES FOR PERFORMANCE
-- #############################################################################

-- Tickets indexes
CALL safe_create_index('idx_tickets_region_status_created', 'tickets', 'region_id, status, created_at');
CALL safe_create_index('idx_tickets_case_code',             'tickets', 'case_code');
CALL safe_create_index('idx_tickets_status_created',        'tickets', 'status, created_at');
CALL safe_create_index('idx_tickets_regional_assignee',     'tickets', 'assigned_region_user_id, status');
CALL safe_create_index('idx_tickets_national_assignee',     'tickets', 'assigned_national_user_id, status');
CALL safe_create_index('idx_tickets_teacher_phone',         'tickets', 'teacher_phone');
CALL safe_create_index('idx_tickets_teacher_staff_id',      'tickets', 'teacher_staff_id');
CALL safe_create_index('idx_tickets_category_priority',     'tickets', 'category_id, priority');
CALL safe_create_index('idx_tickets_sla_due',               'tickets', 'first_response_due_at, resolution_due_at, status');
CALL safe_create_index('idx_tickets_satisfaction',          'tickets', 'satisfaction_rating, status');
CALL safe_create_index('idx_tickets_entry_mode',            'tickets', 'entry_mode, created_at');
CALL safe_create_index('idx_tickets_follow_up',             'tickets', 'next_follow_up_date, status');
CALL safe_create_index('idx_tickets_escalated',             'tickets', 'status, escalated_at');

-- Ticket actions indexes
CALL safe_create_index('idx_actions_ticket_date', 'ticket_actions', 'ticket_id, action_at');
CALL safe_create_index('idx_actions_user_date',   'ticket_actions', 'action_by_user_id, action_at');
CALL safe_create_index('idx_actions_type_date',   'ticket_actions', 'action_type, action_at');

-- Escalations indexes
CALL safe_create_index('idx_escalations_ticket',          'escalations', 'ticket_id');
CALL safe_create_index('idx_escalations_to_user_status',  'escalations', 'escalated_to_user_id, escalation_status');

-- Login audit indexes
CALL safe_create_index('idx_login_audit_email', 'login_audit', 'email, attempted_at');
CALL safe_create_index('idx_login_audit_user',  'login_audit', 'user_id, attempted_at');

-- Login allowlist index
CALL safe_create_index('idx_login_allowlist_email', 'login_allowlist', 'email');

-- Follow-ups indexes
CALL safe_create_index('idx_followups_date_status', 'follow_ups', 'follow_up_date, status');
CALL safe_create_index('idx_followups_ticket',      'follow_ups', 'ticket_id');

-- Templates index
CALL safe_create_index('idx_templates_category', 'case_templates', 'template_category, is_active');


-- #############################################################################
-- SECTION 5: SEED DATA - REGIONS
-- #############################################################################

INSERT IGNORE INTO regions (region_name, region_code) VALUES
('Greater Accra Region',  'GAR'),
('Ashanti Region',        'ASH'),
('Western Region',        'WR'),
('Eastern Region',        'ER'),
('Central Region',        'CR'),
('Northern Region',       'NR'),
('Upper East Region',     'UER'),
('Upper West Region',     'UWR'),
('Volta Region',          'VR'),
('Bono Region',           'BAR'),
('Western North Region',  'WNR'),
('Ahafo Region',          'AHR'),
('Bono East Region',      'BER'),
('Oti Region',            'OR'),
('North East Region',     'NER'),
('Savannah Region',       'SR');


-- #############################################################################
-- SECTION 6: SEED DATA - CHANNELS
-- #############################################################################

INSERT IGNORE INTO channels (channel_name) VALUES
('Regional Phone'),
('Toll-free'),
('Walk-in'),
('Telegram'),
('Facebook'),
('Portal'),
('WhatsApp'),
('Email'),
('Other');


-- #############################################################################
-- SECTION 7: SEED DATA - ISSUE CATEGORIES & SUBCATEGORIES
-- #############################################################################

INSERT IGNORE INTO issue_categories (category_code, category_name, default_national_role, first_response_sla_hours, resolution_sla_hours, escalation_trigger_hours) VALUES
('HR',    'Human Resource Management',                         'Director of HR',                        24, 240, 72),
('DEPLOY','Posting, Deployment and Transfers',                 'Director of HR',                        24, 240, 72),
('PAY',   'Payroll and Compensation',                          'Director of HR',                        24, 240, 72),
('LIC',   'Licensing, Registration and Professional Status',   'Director of Schools and Instruction',   24, 240, 72),
('TLI',   'Teaching and Learning / Instructional Issues',      'Director of Schools and Instruction',   48, 240, 72),
('ADMIN', 'School Administration and Leadership',              'Director of Schools and Instruction',   48, 240, 72),
('WEL',   'Welfare, Safety and Wellbeing',                     'National PRO',                           8,  48, 24),
('DISC',  'Discipline, Misconduct and Grievances',             'Director Legal',                        24, 240, 72),
('PART',  'Partnerships, Affiliations and External Placements','Director of Partnership and Affiliation',48, 360, 72),
('SEC',   'Secondary Education Specific Issues',               'Director Secondary Education',          48, 240, 72),
('BASIC', 'Basic Education Specific Issues',                   'Director Basic Education',              48, 240, 72),
('ICT',   'ICT Systems and Data Issues',                       'National PRO',                          24, 120, 48),
('INFO',  'Information Request / Clarification',               'National PRO',                          48, 120, 48),
('OTHER', 'Other / Not Classified',                            'National PRO',                          48, 240, 72);

-- Subcategories (uses INSERT IGNORE + subselects for idempotency)
INSERT IGNORE INTO issue_subcategories (category_id, subcategory_code, subcategory_name) VALUES
-- HR
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_PROMO',     'Promotion / upgrading'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_CONFIRM',   'Confirmation / regularisation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_LEAVE',     'Leave (annual, study leave, sick leave)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_RETURN',    'Return to post after leave'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_APPRAISAL', 'Appraisal / supervision process (HR side)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_RECORD',    'Personal record correction (DOB, name, grade, etc.)'),
-- DEPLOY
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_POSTING',   'Posting after training / return from leave'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_TRANSFER',  'Transfer request / complaint'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_REPORTING', 'Reporting to duty challenges'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_EXCESS',    'Excess/shortage and redistribution concerns'),
-- PAY
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_MISSING', 'Missing salary'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_DELAY',   'Delayed salary'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_ARREARS', 'Arrears'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_DEDUCT',  'Deductions / SSNIT / loan recovery issues'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_ALLOW',   'Allowances (acting, responsibility, etc.)'),
-- LIC
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_STATUS', 'Licensing status (pending, approved, rejected)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_REG',    'Registration / enrolment delays'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_RENEW',  'Renewal'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_COMPLY', 'Compliance requirements affecting eligibility'),
-- TLI
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_CURR',      'Curriculum implementation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_ASSESS',    'Assessment and grading practices'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_SUBJECT',   'Subject placement / teaching load'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_TIMETABLE', 'Timetabling / contact hours'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_TLM',       'Teaching and learning materials'),
-- ADMIN
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_HEAD',      'Headteacher leadership concern'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_SCHOOLMGT', 'School management dispute/process'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_DIRECTIVE', 'Implementation of directives at school level'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_WORKLOAD',  'Duty allocation and workload disputes'),
-- WEL
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_SAFETY', 'Safety and security threat'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_HARASS', 'Harassment / intimidation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_HEALTH', 'Health / wellbeing support need'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_EMERG',  'Emergency welfare case'),
-- DISC
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_ALLEG', 'Misconduct allegation (teacher)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_PROC',  'Disciplinary process / due process concern'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_GRIEV', 'Formal grievance against supervisor/institution'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_SANCT', 'Sanctions / appeals'),
-- PART
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_AFFIL', 'School affiliation matters'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_FAITH', 'Faith-based / mission school arrangements'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_NGO',   'Partner-supported school issues affecting teachers'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_MOU',   'MoU interpretation affecting operations'),
-- SEC
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_SHS',     'SHS/SHTS policy issue'),
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_BOARD',   'Boarding duty / boarding-related concerns'),
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_SUBCOMB', 'Subject combinations / placement'),
-- BASIC
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_DEPLOY', 'Basic school deployment / multi-grade challenges'),
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_RURAL',  'Rural posting hardship concerns (basic)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_HEAD',   'Basic school leadership/management issue'),
-- ICT
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_PORTAL',  'Portal access issues'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_DATAERR', 'Data error affecting records'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_RESET',   'Password reset / account recovery'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_DEVICE',  'Device/app/platform issue linked to work'),
-- INFO
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_POLICY', 'Policy clarification'),
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_CIRC',   'Circular interpretation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_STATUS', 'Status follow-up on existing case'),
-- OTHER
((SELECT category_id FROM issue_categories WHERE category_code = 'OTHER'), 'OTHER_GEN', 'Not classified (review weekly)');


-- #############################################################################
-- SECTION 8: SEED DATA - STAFF ACCOUNTS
-- Bcrypt hash below corresponds to default initial password.
-- ALL users are forced to change password on first login.
-- #############################################################################

-- Regional Coordinator accounts (one per region)
INSERT INTO users (full_name, email, role, region_id, password_hash, is_active) VALUES
('Greater Accra Regional Coordinator',  'enquiry.greateraccraregion@gmail.com',  'Regional Supervisor', 1,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Ashanti Regional Coordinator',        'enquiry.ashantiregion@gmail.com',       'Regional Supervisor', 2,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Western Regional Coordinator',        'enquiry.westernregion@gmail.com',       'Regional Supervisor', 3,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Eastern Regional Coordinator',        'enquiry.easternregion@gmail.com',       'Regional Supervisor', 4,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Central Regional Coordinator',        'enquiry.centralregion@gmail.com',       'Regional Supervisor', 5,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Northern Regional Coordinator',       'enquiry.northernregion@gmail.com',      'Regional Supervisor', 6,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Upper East Regional Coordinator',     'enquiry.ueastregion@gmail.com',         'Regional Supervisor', 7,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Upper West Regional Coordinator',     'enquiry.uwestregion@gmail.com',         'Regional Supervisor', 8,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Volta Regional Coordinator',          'enquiry.voltaregion@gmail.com',         'Regional Supervisor', 9,  '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Bono Regional Coordinator',           'enquiry.bonoregion@gmail.com',          'Regional Supervisor', 10, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Western North Regional Coordinator',  'enquiry.westernnorthregion@gmail.com',  'Regional Supervisor', 11, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Ahafo Regional Coordinator',          'enquiry.ahaforegion@gmail.com',         'Regional Supervisor', 12, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Bono East Regional Coordinator',      'enquiry.bonoeastregion@gmail.com',      'Regional Supervisor', 13, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Oti Regional Coordinator',            'enquiry.otiregion@gmail.com',           'Regional Supervisor', 14, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('North East Regional Coordinator',     'enquiry.northeastregion@gmail.com',     'Regional Supervisor', 15, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('Savannah Regional Coordinator',       'enquiry.savannahregion@gmail.com',      'Regional Supervisor', 16, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
-- National staff (no region)
('System Administrator',                'enquiry.admin@gmail.com',               'National Admin',    NULL, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1),
('National PRO Office',                 'enquiry.nationalprooffice@gmail.com',   'National Resolver', NULL, '$2a$12$.0q2ipjKMgSpgOQ5MxkJ1OzUllcJ4m.7cx300mqSxiKXkaNgdP8pK', 1)
ON DUPLICATE KEY UPDATE
  full_name     = VALUES(full_name),
  role          = VALUES(role),
  region_id     = VALUES(region_id),
  password_hash = VALUES(password_hash),
  is_active     = 1;

-- Allowlist: ensure all staff emails can log in
INSERT IGNORE INTO login_allowlist (email, is_active, notes) VALUES
('enquiry.greateraccraregion@gmail.com',  TRUE, 'Greater Accra Regional Coordinator'),
('enquiry.ashantiregion@gmail.com',       TRUE, 'Ashanti Regional Coordinator'),
('enquiry.westernregion@gmail.com',       TRUE, 'Western Regional Coordinator'),
('enquiry.easternregion@gmail.com',       TRUE, 'Eastern Regional Coordinator'),
('enquiry.centralregion@gmail.com',       TRUE, 'Central Regional Coordinator'),
('enquiry.northernregion@gmail.com',      TRUE, 'Northern Regional Coordinator'),
('enquiry.ueastregion@gmail.com',         TRUE, 'Upper East Regional Coordinator'),
('enquiry.uwestregion@gmail.com',         TRUE, 'Upper West Regional Coordinator'),
('enquiry.voltaregion@gmail.com',         TRUE, 'Volta Regional Coordinator'),
('enquiry.bonoregion@gmail.com',          TRUE, 'Bono Regional Coordinator'),
('enquiry.westernnorthregion@gmail.com',  TRUE, 'Western North Regional Coordinator'),
('enquiry.ahaforegion@gmail.com',         TRUE, 'Ahafo Regional Coordinator'),
('enquiry.bonoeastregion@gmail.com',      TRUE, 'Bono East Regional Coordinator'),
('enquiry.otiregion@gmail.com',           TRUE, 'Oti Regional Coordinator'),
('enquiry.northeastregion@gmail.com',     TRUE, 'North East Regional Coordinator'),
('enquiry.savannahregion@gmail.com',      TRUE, 'Savannah Regional Coordinator'),
('enquiry.admin@gmail.com',              TRUE, 'System Administrator - National Admin'),
('enquiry.nationalprooffice@gmail.com',  TRUE, 'National PRO Office - escalated cases');

-- SECURITY CLEANUP: Remove the old sample users with invalid password hashes
-- These had $2y$ hashes (PHP bcrypt format) which are incompatible with R's bcrypt package,
-- and one had a plaintext password. They should not exist in the system.
DELETE FROM users WHERE email IN ('john.doe@ges.gov.gh', 'jane.smith@ges.gov.gh', 'robert.johnson@ges.gov.gh', 'admin@ges.gov.gh');
DELETE FROM login_allowlist WHERE email IN ('john.doe@ges.gov.gh', 'jane.smith@ges.gov.gh', 'robert.johnson@ges.gov.gh', 'admin@ges.gov.gh');


-- #############################################################################
-- SECTION 9: TRIGGERS
-- #############################################################################

-- Set SLA dates on ticket creation
DROP TRIGGER IF EXISTS set_ticket_defaults;
DELIMITER //
CREATE TRIGGER set_ticket_defaults
BEFORE INSERT ON tickets
FOR EACH ROW
BEGIN
    DECLARE sla_first_response INT;
    DECLARE sla_resolution INT;

    SELECT first_response_sla_hours, resolution_sla_hours
    INTO sla_first_response, sla_resolution
    FROM issue_categories
    WHERE category_id = NEW.category_id;

    IF NEW.first_response_due_at IS NULL THEN
        SET NEW.first_response_due_at = DATE_ADD(NEW.created_at, INTERVAL sla_first_response HOUR);
    END IF;

    IF NEW.resolution_due_at IS NULL THEN
        SET NEW.resolution_due_at = DATE_ADD(NEW.created_at, INTERVAL sla_resolution HOUR);
    END IF;
END//
DELIMITER ;

-- Auto-log ticket creation
DROP TRIGGER IF EXISTS log_ticket_creation;
DELIMITER //
CREATE TRIGGER log_ticket_creation
AFTER INSERT ON tickets
FOR EACH ROW
BEGIN
    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (NEW.ticket_id, NEW.created_by_user_id, 'create',
            CONCAT('Case logged via ', (SELECT channel_name FROM channels WHERE channel_id = NEW.channel_id)),
            NEW.status);
END//
DELIMITER ;

-- Auto-log status changes
DROP TRIGGER IF EXISTS log_status_changes;
DELIMITER //
CREATE TRIGGER log_status_changes
AFTER UPDATE ON tickets
FOR EACH ROW
BEGIN
    IF OLD.status != NEW.status THEN
        INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, old_status, new_status)
        VALUES (NEW.ticket_id,
                COALESCE(NEW.assigned_region_user_id, NEW.assigned_national_user_id, NEW.created_by_user_id),
                'status_change',
                CONCAT('Status changed from ', OLD.status, ' to ', NEW.status),
                OLD.status, NEW.status);
    END IF;
END//
DELIMITER ;


-- #############################################################################
-- SECTION 10: VIEWS
-- #############################################################################

-- SLA monitoring
CREATE OR REPLACE VIEW sla_monitoring AS
SELECT
    t.ticket_id,
    t.case_code,
    t.created_at,
    r.region_name,
    c.category_name,
    t.priority,
    t.status,
    t.first_response_due_at,
    t.resolution_due_at,
    CASE
        WHEN t.status IN ('New','In Progress') AND t.first_response_at IS NULL AND t.first_response_due_at < NOW()
            THEN 'First Response Overdue'
        WHEN t.status IN ('New','In Progress') AND t.first_response_at IS NULL AND t.first_response_due_at < DATE_ADD(NOW(), INTERVAL 2 HOUR)
            THEN 'First Response Due Soon'
        WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at < NOW()
            THEN 'Resolution Overdue'
        WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at < DATE_ADD(NOW(), INTERVAL 4 HOUR)
            THEN 'Resolution Due Soon'
        ELSE 'On Track'
    END as sla_status,
    TIMESTAMPDIFF(HOUR, t.created_at, NOW()) as hours_open,
    u1.full_name as regional_assignee,
    u2.full_name as national_assignee
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN users u1 ON t.assigned_region_user_id = u1.user_id
LEFT JOIN users u2 ON t.assigned_national_user_id = u2.user_id
WHERE t.status NOT IN ('Closed');

-- Dashboard statistics (standalone, no nested view reference)
CREATE OR REPLACE VIEW dashboard_stats AS
SELECT
    r.region_name,
    DATE(t.created_at) as case_date,
    t.status,
    t.priority,
    c.category_name,
    c.category_code,
    COUNT(*) as case_count,
    AVG(TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, NOW()))) as avg_hours_to_resolve,
    COUNT(CASE WHEN t.resolved_at IS NOT NULL THEN 1 END) as resolved_count,
    COUNT(CASE WHEN t.status = 'Escalated' THEN 1 END) as escalated_count,
    COUNT(CASE WHEN t.status NOT IN ('Resolved','Closed')
                AND t.resolution_due_at IS NOT NULL
                AND t.resolution_due_at < NOW() THEN 1 END) as overdue_count
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
GROUP BY r.region_name, DATE(t.created_at), t.status, t.priority, c.category_name, c.category_code;

-- Pending follow-ups
CREATE OR REPLACE VIEW pending_follow_ups AS
SELECT
    f.follow_up_id,
    f.ticket_id,
    t.case_code,
    t.teacher_name,
    t.teacher_phone,
    t.status as ticket_status,
    r.region_name,
    c.category_name,
    f.follow_up_date,
    f.follow_up_notes,
    f.status as follow_up_status,
    CASE
        WHEN f.follow_up_date < CURDATE() THEN 'Overdue'
        WHEN f.follow_up_date = CURDATE() THEN 'Due Today'
        ELSE 'Upcoming'
    END as urgency,
    DATEDIFF(CURDATE(), f.follow_up_date) as days_overdue,
    u.full_name as created_by
FROM follow_ups f
JOIN tickets t ON f.ticket_id = t.ticket_id
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN users u ON f.created_by_user_id = u.user_id
WHERE f.status = 'Pending'
ORDER BY f.follow_up_date ASC;

-- Escalated cases
CREATE OR REPLACE VIEW escalated_cases_view AS
SELECT
    t.ticket_id,
    t.case_code,
    t.created_at,
    t.escalated_at,
    t.teacher_name,
    t.teacher_phone,
    t.teacher_staff_id,
    t.school_name,
    t.district,
    r.region_name,
    c.category_name,
    t.priority,
    t.status,
    t.summary,
    t.description,
    t.escalation_reason,
    TIMESTAMPDIFF(HOUR, t.escalated_at, NOW()) as hours_since_escalation,
    e.escalation_reason as detailed_reason,
    e.escalation_status,
    u_by.full_name as escalated_by,
    u_to.full_name as escalated_to
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN escalations e ON t.ticket_id = e.ticket_id
LEFT JOIN users u_by ON e.escalated_by_user_id = u_by.user_id
LEFT JOIN users u_to ON e.escalated_to_user_id = u_to.user_id
WHERE t.status = 'Escalated'
ORDER BY t.escalated_at DESC;


-- #############################################################################
-- SECTION 11: STORED PROCEDURES
-- #############################################################################

-- Escalate a ticket (transactional)
DROP PROCEDURE IF EXISTS EscalateTicket;
DELIMITER //
CREATE PROCEDURE EscalateTicket(
    IN p_ticket_id           BIGINT,
    IN p_escalated_by_user_id INT,
    IN p_escalated_to_user_id INT,
    IN p_reason              TEXT
)
BEGIN
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
        RESIGNAL;
    END;

    START TRANSACTION;

    UPDATE tickets
    SET status = 'Escalated',
        assigned_national_user_id = p_escalated_to_user_id,
        escalated_at = NOW(),
        escalation_reason = p_reason
    WHERE ticket_id = p_ticket_id;

    INSERT INTO escalations (ticket_id, escalated_by_user_id, escalated_to_user_id, escalation_reason)
    VALUES (p_ticket_id, p_escalated_by_user_id, p_escalated_to_user_id, p_reason);

    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (p_ticket_id, p_escalated_by_user_id, 'escalation', p_reason, 'Escalated');

    COMMIT;
END//
DELIMITER ;

-- Resolve a ticket (transactional)
DROP PROCEDURE IF EXISTS ResolveTicket;
DELIMITER //
CREATE PROCEDURE ResolveTicket(
    IN p_ticket_id            BIGINT,
    IN p_resolved_by_user_id  INT,
    IN p_resolution_notes     TEXT
)
BEGIN
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
        RESIGNAL;
    END;

    START TRANSACTION;

    UPDATE tickets
    SET status = 'Resolved',
        resolved_at = NOW()
    WHERE ticket_id = p_ticket_id;

    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (p_ticket_id, p_resolved_by_user_id, 'resolution', p_resolution_notes, 'Resolved');

    COMMIT;
END//
DELIMITER ;


-- #############################################################################
-- SECTION 12: SEED DATA - CASE TEMPLATES
-- #############################################################################

INSERT IGNORE INTO case_templates (template_name, template_category, template_subject, template_body) VALUES

-- Payroll
('Salary Delay Acknowledgement', 'Payroll', 'Acknowledgement - Salary Payment Issue',
'Dear Teacher,
Thank you for contacting the GES Teacher Helpline regarding your salary concern.
I have logged your case as a payroll query and escalated it to the Payroll Unit. Your reference number is {case_code}.
The Payroll Unit will respond within 24 working hours. Please keep this reference number for future follow-up.
Is there anything else I can help you with today?
Best regards,
GES Teacher Helpline'),

('Salary Verification Request', 'Payroll', 'Information Required - Salary Query',
'Dear Teacher,
To assist with your salary issue, we need to verify the following:
- Staff ID
- Number of months salary is delayed
- Bank used for salary payments
- Have you recently changed your phone number or bank details?
Please provide these details so we can escalate to the HR Payroll Unit.
Typical resolution time is 7-30 days. We will keep you updated on progress.
Best regards,
GES Teacher Helpline'),

-- CPD/Licensing
('CPD Enquiry Response', 'CPD/Licensing', 'Response - CPD/Licensing Query',
'Dear Teacher,
Thank you for your query about CPD/Licensing.
{response_details}
For further assistance, please visit the Teacher Portal or contact your District Education Office.
Your reference number is {case_code}.
Best regards,
GES Teacher Helpline'),

-- ICT
('Portal Login Issue', 'ICT', 'Response - Portal Access Issue',
'Dear Teacher,
I understand you are having difficulty accessing the Teacher Portal.
Please try the following steps:
1. Clear your browser cache and cookies
2. Try using a different browser (Chrome, Firefox, Edge)
3. Ensure you are using the correct Staff ID
4. If you forgot your password, use the "Forgot Password" link
If the issue persists, please provide your Staff ID and we will escalate to the ICT Unit for password reset.
Your reference number is {case_code}.
Best regards,
GES Teacher Helpline'),

-- Transfer
('Transfer Request Acknowledgement', 'Transfer', 'Acknowledgement - Transfer Request Query',
'Dear Teacher,
Thank you for contacting us regarding your transfer request.
I have logged your concern and will escalate to the HR Directorate. Your reference number is {case_code}.
Please note:
- Transfer requests are processed according to GES policy and available vacancies
- The District HR will be notified of your request
- You will receive an update within 3-5 working days
Best regards,
GES Teacher Helpline'),

-- Welfare
('Welfare Case Acknowledgement', 'Welfare', 'Urgent - Welfare Case Logged',
'Dear Teacher,
I am sorry to hear about your situation. I have logged this as a high-priority welfare case.
Your reference number is {case_code}.
The HR Directorate will contact you within 4 working hours. If you are in immediate danger, please contact local emergency services.
We take your safety seriously and will ensure this matter is addressed promptly.
Best regards,
GES Teacher Helpline'),

('Harassment Report Acknowledgement', 'Welfare', 'Confidential - Report Received',
'Dear Teacher,
Thank you for trusting us with this sensitive matter. I have recorded your report as a confidential welfare case.
Your reference number is {case_code}.
This has been escalated immediately to the HR Directorate and Legal Unit as a high-priority case. You will be contacted within 4 working hours.
Please note: All information shared is treated with strict confidentiality.
If you are in immediate danger, please call local emergency services.
Best regards,
GES Teacher Helpline'),

-- General
('Case Resolved SMS', 'General', 'Case Resolved Notification',
'GES Helpline: Your case (Ref: {case_code}) has been resolved. If you need further help, call us on this helpline.'),

('Case Acknowledgement SMS', 'General', 'Case Received Notification',
'GES Helpline: Your request (Ref: {case_code}) has been received and escalated to the appropriate office. You will receive an update within {timeframe}. Thank you.'),

('Follow-up Required', 'General', 'Follow-up Scheduled',
'Dear Teacher,
Thank you for your patience. A follow-up has been scheduled for {follow_up_date} regarding your case (Ref: {case_code}).
We will contact you on that date with an update. If you have any urgent concerns before then, please call the helpline and reference your case number.
Best regards,
GES Teacher Helpline'),

-- Escalation
('Escalation to HR', 'Escalation', 'Case Escalated to HR Directorate',
'Dear HR Directorate,
A case has been escalated from the Teacher Helpline that requires your attention.
Case Reference: {case_code}
Caller: {teacher_name}
Region: {region}
Category: {category}
Priority: {priority}
Issue Summary:
{summary}
Escalation Reason:
{escalation_reason}
Please respond within 24-72 hours depending on priority.
Best regards,
Regional Helpline Team'),

('Escalation to Legal', 'Escalation', 'Case Escalated to Legal Unit',
'Dear Legal Unit,
A case requiring legal attention has been escalated from the Teacher Helpline.
Case Reference: {case_code}
Caller: {teacher_name}
Region: {region}
Category: Discipline/Misconduct/Grievance
Priority: {priority}
Issue Summary:
{summary}
Please treat this matter with urgency and confidentiality.
Best regards,
Regional Helpline Team');


-- #############################################################################
-- SECTION 13: CLEANUP HELPERS
-- #############################################################################

DROP PROCEDURE IF EXISTS safe_add_column;
DROP PROCEDURE IF EXISTS safe_create_index;

SELECT 'GES Teacher Support Helpline - Schema v8 setup complete!' AS Status;









