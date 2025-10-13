import {MigrationInterface, QueryRunner} from "typeorm";

export class Init1607317681404 implements MigrationInterface {
    name = 'Init1607317681404'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "public"."author" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "name" character varying NOT NULL, CONSTRAINT "PK_cbe28d6029d9395ff69e36a97c1" PRIMARY KEY ("id"))`);
        await queryRunner.query(`CREATE TABLE "public"."photo_metadata" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "height" integer NOT NULL, "width" integer NOT NULL, "orientation" character varying NOT NULL, "compressed" boolean NOT NULL, "comment" character varying NOT NULL, "photo_id" uuid, CONSTRAINT "REL_4d37b2e7ccfa55525d0391a757" UNIQUE ("photo_id"), CONSTRAINT "PK_79f41a4df3849e41616311dbe6f" PRIMARY KEY ("id"))`);
        await queryRunner.query(`CREATE TABLE "public"."photo" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "name" character varying(100) NOT NULL, "description" text NOT NULL, "filename" character varying NOT NULL, "views" integer NOT NULL DEFAULT '0', "is_published" boolean NOT NULL, "author_id" uuid, CONSTRAINT "PK_006f41e65eb5601b7e7338e47d5" PRIMARY KEY ("id"))`);
        await queryRunner.query(`CREATE TABLE "public"."album" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "name" character varying NOT NULL, CONSTRAINT "PK_5152808754c3ddc024bac74fe22" PRIMARY KEY ("id"))`);
        await queryRunner.query(`CREATE TABLE "public"."post" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "title" character varying NOT NULL, CONSTRAINT "PK_b8ef5e0e707b097e2ea177daacf" PRIMARY KEY ("id"))`);
        await queryRunner.query(`CREATE TABLE "public"."album & photo" ("album_id" uuid NOT NULL, "photo_id" uuid NOT NULL, CONSTRAINT "PK_1b8dfb40b0be447aa51a6906746" PRIMARY KEY ("album_id", "photo_id"))`);
        await queryRunner.query(`CREATE INDEX "IDX_f834af58c551fbf49ec758589f" ON "public"."album & photo" ("album_id") `);
        await queryRunner.query(`CREATE INDEX "IDX_636b1e1bef31e57831c00f7353" ON "public"."album & photo" ("photo_id") `);
        await queryRunner.query(`ALTER TABLE "public"."photo_metadata" ADD CONSTRAINT "FK_4d37b2e7ccfa55525d0391a757e" FOREIGN KEY ("photo_id") REFERENCES "public"."photo"("id") ON DELETE CASCADE ON UPDATE NO ACTION`);
        await queryRunner.query(`ALTER TABLE "public"."photo" ADD CONSTRAINT "FK_928712b49c3ccbc07398e0d2a57" FOREIGN KEY ("author_id") REFERENCES "public"."author"("id") ON DELETE CASCADE ON UPDATE NO ACTION`);
        await queryRunner.query(`ALTER TABLE "public"."album & photo" ADD CONSTRAINT "FK_f834af58c551fbf49ec758589f2" FOREIGN KEY ("album_id") REFERENCES "public"."album"("id") ON DELETE CASCADE ON UPDATE NO ACTION`);
        await queryRunner.query(`ALTER TABLE "public"."album & photo" ADD CONSTRAINT "FK_636b1e1bef31e57831c00f7353d" FOREIGN KEY ("photo_id") REFERENCES "public"."photo"("id") ON DELETE CASCADE ON UPDATE NO ACTION`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "public"."album & photo" DROP CONSTRAINT "FK_636b1e1bef31e57831c00f7353d"`);
        await queryRunner.query(`ALTER TABLE "public"."album & photo" DROP CONSTRAINT "FK_f834af58c551fbf49ec758589f2"`);
        await queryRunner.query(`ALTER TABLE "public"."photo" DROP CONSTRAINT "FK_928712b49c3ccbc07398e0d2a57"`);
        await queryRunner.query(`ALTER TABLE "public"."photo_metadata" DROP CONSTRAINT "FK_4d37b2e7ccfa55525d0391a757e"`);
        await queryRunner.query(`DROP INDEX "public"."IDX_636b1e1bef31e57831c00f7353"`);
        await queryRunner.query(`DROP INDEX "public"."IDX_f834af58c551fbf49ec758589f"`);
        await queryRunner.query(`DROP TABLE "public"."album & photo"`);
        await queryRunner.query(`DROP TABLE "public"."post"`);
        await queryRunner.query(`DROP TABLE "public"."album"`);
        await queryRunner.query(`DROP TABLE "public"."photo"`);
        await queryRunner.query(`DROP TABLE "public"."photo_metadata"`);
        await queryRunner.query(`DROP TABLE "public"."author"`);
    }

}
